{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10157: AstaIOFilter.pas 
{
{   Rev 1.0    4/10/2003 6:30:56 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:14 PM  Steve    Version: 1.505
}
unit AstaIOFilter;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses Classes, DBCommon, DB, TypInfo, SysUtils
{$IFDEF Delphi6andUp}
   , Variants
{$ENDIF}
{$IFDEF LINUX}
   , SqlExpr
{$ENDIF}
  ;

const

{ Field Types (Logical) }
  FldTypeMap: TFieldMap =
  (ord(ftUnknown),
   ord(ftString),
   ord(ftSmallint),
   ord(ftInteger),
   ord(ftWord),
   ord(ftBoolean),
   ord(ftFloat),
   ord(ftCurrency),
   ord(ftBCD),
   ord(ftDate),
   ord(ftTime),
   ord(ftDateTime),
   ord(ftBytes),
   ord(ftVarBytes),
   ord(ftAutoInc),
   ord(ftBlob),
   ord(ftMemo),
   ord(ftGraphic),
   ord(ftFmtMemo),
   ord(ftParadoxOle),
   ord(ftDBaseOle),
   ord(ftTypedBinary),
   ord(ftCursor),
   ord(ftString),
   ord(ftString),
   ord(ftLargeint),
   ord(ftADT),
   ord(ftArray),
   ord(ftReference),
   ord(ftDataSet),
   ord(ftOraBlob),
   ord(ftOraClob),
   ord(ftVariant),
   ord(ftInterface),
   ord(ftIDispatch),
   ord(ftGuid)
{$IFDEF Delphi6AndUp}
 , ord(ftTimeStamp),
   ord(ftFMTBcd)
{$ENDIF}
{$IFDEF Delphi12AndUp}
 , ord(ftUnknown), ord(ftUnknown), ord(ftUnknown), ord(ftUnknown)
 , ord(ftUnknown), ord(ftUnknown), ord(ftUnknown), ord(ftUnknown)
 , ord(ftUnknown), ord(ftUnknown), ord(ftUnknown), ord(ftUnknown)
 , ord(ftUnknown), ord(ftUnknown)
{$ENDIF}
   );

type
  ppCANExpr = ^pCANExpr;
  pCANExpr = ^CANExpr;
  CANExpr = packed record { Expression Tree }
    iVer: Word; { Version tag of expression. }
    iTotalSize: Word; { Size of this structure }
    iNodes: Word; { Number of nodes }
    iNodeStart: Word; { Starting offet of Nodes in this }
    iLiteralStart: Word; { Starting offset of Literals in this }
  end;

  pCANOp = ^CANOp;
  CANOp = (
    canNOTDEFINED, {                                  (*) }
    canISBLANK, { CANUnary;  is operand blank.     (*) }
    canNOTBLANK, { CANUnary;  is operand not blank. (*) }
    canEQ, { CANBinary, CANCompare; equal.    (*) }
    canNE, { CANBinary; NOT equal.            (*) }
    canGT, { CANBinary; greater than.         (*) }
    canLT, { CANBinary; less than.            (*) }
    canGE, { CANBinary; greater or equal.     (*) }
    canLE, { CANBinary; less or equal.        (*) }
    canNOT, { CANUnary; NOT                    (*) }
    canAND, { CANBinary; AND                   (*) }
    canOR, { CANBinary; OR                    (*) }
    canTUPLE2, { CANUnary; Entire record is operand. }
    canFIELD2, { CANUnary; operand is field       (*) }
    canCONST2, { CANUnary; operand is constant    (*) }
    canMINUS, { CANUnary;  minus. }
    canADD, { CANBinary; addition. }
    canSUB, { CANBinary; subtraction. }
    canMUL, { CANBinary; multiplication. }
    canDIV, { CANBinary; division. }
    canMOD, { CANBinary; modulo division. }
    canREM, { CANBinary; remainder of division. }
    canSUM, { CANBinary, accumulate sum of. }
    canCOUNT, { CANBinary, accumulate count of. }
    canMIN, { CANBinary, find minimum of. }
    canMAX, { CANBinary, find maximum of. }
    canAVG, { CANBinary, find average of. }
    canCONT, { CANBinary; provides a link between two }
    canUDF2, { CANBinary; invokes a User defined fn }
    canCONTINUE2, { CANUnary; Stops evaluating records }
    canLIKE, { CANCompare, extended binary compare       (*) }
    canIN, { CANBinary field in list of values }
    canLIST2, { List of constant values of same type }
    canUPPER, { CANUnary: upper case }
    canLOWER, { CANUnary: lower case }
    canFUNC2, { CANFunc: Function }
    canLISTELEM2, { CANListElem: List Element }
    canASSIGN { CANBinary: Field assignment }
  );

  NODEClass = ({ Node Class }
    nodeNULL, { Null node                  (*) }
    nodeUNARY, { Node is a unary            (*) }
    nodeBINARY, { Node is a binary           (*) }
    nodeCOMPARE, { Node is a compare          (*) }
    nodeFIELD, { Node is a field            (*) }
    nodeCONST, { Node is a constant         (*) }
    nodeTUPLE, { Node is a record }
    nodeCONTINUE, { Node is a continue node    (*) }
    nodeUDF, { Node is a UDF node }
    nodeLIST, { Node is a LIST node }
    nodeFUNC, { Node is a Function node }
    nodeLISTELEM { Node is a List Element node }
  );

  { NODE definitions including misc data structures }
  {-------------------------------------------------}

  pCANHdr = ^CANHdr;
  CANHdr = packed record { Header part common to all     (*) }
    nodeClass: NODEClass;
    spacer1: Byte; // spacer
    spacer2: Byte; // spacer
    spacer3: Byte; // spacer
    canOp: CANOp;
    spacer4: Byte; // spacer
    spacer5: Byte; // spacer
    spacer6: Byte; // spacer
  end;

  pCANUnary = ^CANUnary;
  CANUnary = packed record { Unary Node                    (*) }
    nodeClass: NODEClass;
    spacer1: Byte; // spacer
    spacer2: Byte; // spacer
    spacer3: Byte; // spacer
    canOp: CANOp;
    spacer4: Byte; // spacer
    spacer5: Byte; // spacer
    spacer6: Byte; // spacer
    iOperand1: Word; { Byte offset of Operand node }
  end;

  pCANBinary = ^CANBinary;
  CANBinary = packed record { Binary Node                   (*) }
    nodeClass: NODEClass;
    spacer1: Byte; // spacer
    spacer2: Byte; // spacer
    spacer3: Byte; // spacer
    canOp: CANOp;
    spacer4: Byte; // spacer
    spacer5: Byte; // spacer
    spacer6: Byte; // spacer
    iOperand1: Word; { Byte offset of Op1 }
    iOperand2: Word; { Byte offset of Op2 }
  end;

  pCANField = ^CANField;
  CANField = packed record { Field }
    nodeClass: NODEClass;
    spacer1: Byte; // spacer
    spacer2: Byte; // spacer
    spacer3: Byte; // spacer
    canOp: CANOp;
    spacer4: Byte; // spacer
    spacer5: Byte; // spacer
    spacer6: Byte; // spacer
    iFieldNum: Word;
    iNameOffset: Word;                  { Name offset in Literal pool }
  end;

  pCANConst = ^CANConst;
  CANConst = packed record { Constant }
    nodeClass: NODEClass;
    spacer1: Byte; // spacer
    spacer2: Byte; // spacer
    spacer3: Byte; // spacer
    canOp: CANOp;
    spacer4: Byte; // spacer
    spacer5: Byte; // spacer
    spacer6: Byte; // spacer
    iType: Word; { Constant type. }
    iSize: Word; { Constant size. (in bytes) }
    iOffset: Word; { Offset in the literal pool. }
  end;

  pCANTuple = ^CANTuple;
  CANTuple = packed record { Tuple (record) }
    nodeClass: NODEClass;
    spacer1: Byte; // spacer
    spacer2: Byte; // spacer
    spacer3: Byte; // spacer
    canOp: CANOp;
    spacer4: Byte; // spacer
    spacer5: Byte; // spacer
    spacer6: Byte; // spacer
    iSize: Word; { Record size. (in bytes) }
  end;

  pCANContinue = ^CANContinue;
  CANContinue = packed record { Break Node                    (*) }
    nodeClass: NODEClass;
    spacer1: Byte; // spacer
    spacer2: Byte; // spacer
    spacer3: Byte; // spacer
    canOp: CANOp;
    spacer4: Byte; // spacer
    spacer5: Byte; // spacer
    spacer6: Byte; // spacer
    iContOperand: Word; { Continue if operand is true. }
  end;

  pCANCompare = ^CANCompare;
  CANCompare = packed record { Extended compare Node (text fields) (*) }
    nodeClass: NODEClass;
    spacer1: Byte; // spacer
    spacer2: Byte; // spacer
    spacer3: Byte; // spacer
    canOp: CANOp; { canLIKE, canEQ }
    spacer4: Byte; // spacer
    spacer5: Byte; // spacer
    spacer6: Byte; // spacer
    bCaseInsensitive: WordBool; { 3 val: UNKNOWN = "fastest", "native" }
    iPartialLen: Word; { Partial fieldlength (0 is full length) }
    iOperand1: Word; { Byte offset of Op1 }
    iOperand2: Word; { Byte offset of Op2 }
  end;

  pCANFunc = ^CANFunc;
  CANFunc = packed record { Function }
    nodeClass: NODEClass;
    spacer1: Byte; // spacer
    spacer2: Byte; // spacer
    spacer3: Byte; // spacer
    canOp: CANOp;
    spacer4: Byte; // spacer
    spacer5: Byte; // spacer
    spacer6: Byte; // spacer
    iNameOffset: Word; { Name offset in Literal pool }
    iElemOffset: Word; { Offset of first List Element in Node pool }
  end;

  pCANListElem = ^CANListElem;
  CANListElem = packed record { List Element }
    nodeClass: NODEClass;
    spacer1: Byte; // spacer
    spacer2: Byte; // spacer
    spacer3: Byte; // spacer
    canOp: CANOp;
    spacer4: Byte; // spacer
    spacer5: Byte; // spacer
    spacer6: Byte; // spacer
    iOffset: Word; { Arg offset in Node pool }
    iNextOffset: Word; { Offset in Node pool of next ListElem or 0 if end of list }
  end;

{This is the node to be used to pass User defined functions }
const
  iLangSQL = 0; { Common SQL dialect }
  iDbaseExpr = 2; { This is also the driver ID for dBASE }

type
  pCANUdf = ^CANUdf;
  CANUdf = packed record { A user defined function }
    nodeClass: NODEClass;
    canOp: CANOp;
    iOffSzFuncName: Word; { Offset in literal pool to Function Name string(0 terminated) }
    iOperands: Word; { Byte offset of Operands (concatenated using canCONT) }
    iDrvDialect: Word; { Driver Dialect ID for UDF string supplied }
    iOffSzUDF: Word; { Offset in literal pool to UDF string (0 terminated) }
  end;

  pCANList = ^CANList;
  CANList = packed record { List of Constants }
    nodeClass: NODEClass;
    canOp: CANOp;
    iType: Word; { Constant type. }
    iTotalSize: Word; { Total list size; }
    iElemSize: Word; { Size of each elem for fix-width types }
    iElems: Word; { Number of elements in list }
    iOffset: Word; { Offset in the literal pool to first elem. }
  end;

  pCANNode = ^CANNode;
  CANNode = packed record
    case Integer of
      0: (canHdr: CANHdr);
      1: (canUnary: CANUnary);
      2: (canBinary: CANBinary);
      3: (canField: CANField);
      4: (canConst: CANConst);
      5: (canTuple: CANTuple);
      6: (canContinue: CANContinue);
      7: (canCompare: CANCompare);
      8: (canList: CANList);
      9: (canFunc: CANFunc);
      10: (canListElem: CANListElem);
  end;

  TAstaExpressionUsage = (fuFilter, fuFieldCheck, fuRecordCheck, fuFieldDefault);
  TAstaExpressionUsages = set of TAstaExpressionUsage;

  TAstaExpressionTreeReader = class
  private
    FParser: TExprParser;
    FDataSet: TDataSet;
    FParsOpts: TParserOptions;
    FText: String;
    FOptions: TFilterOptions;
    FFieldName: String;
  protected
    function TotalSize: Integer;
    function Nodecount: Integer;
    function NodeStart: Integer;
    function LiteralStart: Integer;
    function TreeHeader: CanHdr;
    function Offset(Offset:LongInt):Pointer;
    function InternalEvaluate(AOffset: LongInt; var AIsNull: Integer): Variant;
    function DoFunctionCall(const AFunctionName: String; const AArgs: Variant): Variant;
  public
    constructor Create(ADataSet: TDataSet; const AExpression: string;
      AOptions: TFilterOptions; AUsage: TAstaExpressionUsage; AFieldName: String);
    destructor Destroy; override;
    procedure Parse;
    function Evaluate: Variant;
    property Text: String read FText;
    property Options: TFilterOptions read FOptions;
  end;

implementation

uses AstaIODBList, AstaIOResources, AstaIOUtil
{$ifdef Delphi6AndUP}
  , FmtBCD, SqlTimSt
{$endif}
  ;

constructor TAstaExpressionTreeReader.Create(ADataSet: TDataSet; const AExpression: string;
  AOptions: TFilterOptions; AUsage: TAstaExpressionUsage; AFieldName: String);
begin
  inherited Create;
  FParsOpts := [poExtSyntax];
  if AUsage = fuFieldDefault then begin
    FParsOpts := FParsOpts + [poDefaultExpr];
    FFieldName := AFieldName;
  end
  else
    FFieldName := '';
  FDataSet := ADataSet;
  FText := AExpression;
  FOptions := AOptions;
end;

destructor TAstaExpressionTreeReader.Destroy;
begin
  if FParser <> nil then begin
    FParser.Free;
    FParser := nil;
  end;
  FDataSet := nil;
  inherited Destroy;
end;

{$HINTS OFF}
type
  __TField = class(TComponent)
  private
    FAutoGenerateValue: TAutoRefreshFlag;
    FDataSet: TDataSet;
    FFieldName: string;
    FFields: TFields;
    FDataType: TFieldType;
    FReadOnly: Boolean;
    FFieldKind: TFieldKind;
  end;
{$HINTS ON}

procedure TAstaExpressionTreeReader.Parse;
var
  fld: TField;
  restFieldKind: Boolean;
begin
  if FParser = nil then begin
    restFieldKind := False;
    fld := nil;
    if poDefaultExpr in FParsOpts then begin
      fld := FDataSet.FieldByName(FFieldName);
      if fld.FieldKind = fkCalculated then begin
        restFieldKind := True;
        __TField(fld).FFieldKind := fkData;
      end;
    end;
    try
      FParser := TExprParser.Create(FDataSet, FText, FOptions, FParsOpts,
        FFieldName, nil, FldTypeMap);
    finally
      if restFieldKind then
        __TField(fld).FFieldKind := fkCalculated;
    end;
  end;
end;

function TAstaExpressionTreeReader.TotalSize: Integer;
begin
  result := PCanExpr(FParser.Filterdata).iTotalSize;
end;

function TAstaExpressionTreeReader.Nodecount: Integer;
begin
  result := PcanExpr(FParser.FilterData).iNodes;
end;

function TAstaExpressionTreeReader.NodeStart: Integer;
begin
  result := PcanExpr(FParser.FilterData).iNodeStart;
end;

function TAstaExpressionTreeReader.LiteralStart: Integer;
begin
  result := PcanExpr(FParser.FilterData).iLiteralStart;
end;

function TAstaExpressionTreeReader.TreeHeader: CanHdr;
begin
  result := CanHdr(OffsetPointer(PcanExpr(FParser.FilterData), PCanExpr(FParser.FilterData).iNodeStart)^);
end;

function TAstaExpressionTreeReader.Offset(Offset:LongInt):Pointer;
begin
  result := Pointer(LongInt(PcanExpr(FParser.FilterData)) + Offset);
end;

function TAstaExpressionTreeReader.Evaluate:Variant;
var
  IsNull: Integer;
begin
  result := InternalEvaluate(0, IsNull);
end;

function TAstaExpressionTreeReader.InternalEvaluate(AOffset: LongInt; var AIsNull: Integer): Variant;

  function StringMatch(const v1, v2: Variant; ANoCase: Boolean;
    APartial: Boolean; APartialLength: Integer): Integer;
  var
    s1, s2: String;
  begin
    s1 := VarToStr(v1);
    s2 := VarToStr(v2);
    if APartial then
      if ANoCase then
        Result := AnsiCompareText(Copy(s1, 1, APartialLength), Copy(s2, 1, APartialLength))
      else
        Result := AnsiCompareStr(Copy(s1, 1, APartialLength), Copy(s2, 1, APartialLength))
    else
      if ANoCase then
        Result := AnsiCompareText(s1, s2)
      else
        Result := AnsiCompareStr(s1, s2);
  end;

  function Match(const v1, v2: Variant): Integer;
  begin
    if VarIsNull(v1) and VarIsNull(v2) then
      Result := 0
    else if VarIsNull(v1) or VarIsNull(v2) then
      Result := -1
    else
      try
        if v1 = v2 then
          Result := 0
        else if v1 > v2 then
          Result := 1
        else
          Result := -1;
      except
        Result := -1;
      end;
  end;

  function GetString(ANameOffset: Word; ALen: Word): String;
  var
    pCh: PChar;
    ln: Integer;
  begin
    pCh := Offset(ANameOffset + LiteralStart);
    Result := '';
    ln := 0;
    while (pCh^ <> #0) and ((ALen = $FFFF) or (ln < ALen)) do begin
      Result := Result + pCh^;
      Inc(pCh);
      Inc(ln);
    end;
  end;

  function GetField(pCanF: pCANField): TField;
  var
    nActualField, ni: Integer;
  begin
    if SmallInt(pCanF^.iFieldNum) >= 0 then begin
      nActualField := -1;
      for ni := 0 to FDataSet.Fieldcount - 1 do
        if FDataSet.Fields[ni].FieldNo = pCanF^.iFieldNum then begin
          nActualField := ni;
          break;
        end;
      if nActualField = -1 then
        raise EDataBaseError.Create('Field does not found:' + IntToStr(pCanF^.iFieldNum));
      Result := FDataSet.Fields[nActualField];
    end
    else
      Result := FDataSet.FieldByName(GetString(pCanF^.iNameOffset, $FFFF));
  end;

  function IsBlank(const v: Variant; var AIsNull: Integer): Boolean;
  begin
    result := False;
    if AIsNull = 1 then
      result := True
    else begin
      if VarAsType(v,varString)='' then
        result := True;
    end;
  end;

  function IsLike(const v1, v2: Variant; ANoCase: Boolean): Boolean;

    function DoLike(pStr, pMask: PChar): Boolean;
    begin
      Result := True;
      while ((pStr^ = pMask^) and (pMask^ <> '%') or (pMask^ = '_')) and
            (pStr^ <> #0) and (pMask^ <> #0) do begin
        Inc(pMask);
        Inc(pStr);
      end;
      if pMask^ = '%' then begin
        while (pMask^ = '%') or (pMask^ = '_') do begin
          if pMask^ = '_' then
            if pStr^ = #0 then begin
              Result := False;
              Exit;
            end
            else
              Inc(pStr);
          Inc(pMask);
        end;
        if pMask^ <> #0 then begin
          while (pStr^ <> #0) and ((pMask^ <> pStr^) or not DoLike(pStr, pMask)) do
            Inc(pStr);
          Result := pStr^ <> #0;
        end;
      end
      else
        Result := (pMask^ = #0) and (pStr^ = #0);
    end;

  var
    sMask, sStr: String;
  begin
    if VarIsNull(v1) or VarIsNull(v2) then
      Result := False
    else begin
      sMask := v2;
      sStr := v1;
      if sMask = '%' then
        Result := True
      else if (sMask = '') or (sStr = '') then
        Result := False
      else begin
        if ANoCase then
          Result := DoLike(PChar(AnsiUpperCase(sStr)), PChar(AnsiUpperCase(sMask)))
        else
          Result := DoLike(PChar(sStr), PChar(sMask));
      end;
    end;
  end;

  function ProcessUnaryNode(pCanU: pCanUnary): Variant;
  begin
    case pCanU^.CanOP of
    canISBLANK :         { CANUnary;  is operand blank.     (*) }
      Result := IsBlank(InternalEvaluate(pCanU^.iOperand1, AIsNull), AIsNull);
    canNOTBLANK :        { CANUnary;  is operand not blank. (*) }
      Result := not IsBlank(InternalEvaluate(pCanU^.iOperand1, AIsNull), AIsNull);
    canNOT :             { CANUnary; NOT                    (*) }
      Result := not InternalEvaluate(pCanU^.iOperand1, AIsNull);
    canMINUS :           { CANUnary;  minus. }
      Result := - InternalEvaluate(pCanU^.iOperand1, AIsNull);
    else
      raise EDataBaseError.Create(SMissingUnaryFilterEval + IntToStr(Ord(pCanU^.CanOP)));

{   canUPPER :          { CANUnary: upper case
      Result := UpperCase(InternalEvaluate(pCanU^.iOperand1));
    canLOWER :          { CANUnary: lower case
      Result := LowerCase(InternalEvaluate(pCanU^.iOperand1));}

{   canTUPLE2,                          { CANUnary; Entire record is operand.
    canFIELD2,                          { CANUnary; operand is field       (*)
    canCONST2,                          { CANUnary; operand is constant    (*)
    canCONTINUE2,                       { CANUnary; Stops evaluating records }
    end;
  end;

  function ProcessBinaryNode(pCanB: pCanBinary): Variant;
  var
    fld: TField;
    pCanL: pCanListElem;
    pCanF: pCanField;
    v1, v2: Variant;
  begin
    case pCanB^.CanOP of
    canLT:
      begin    { CANBinary; less than.            (*) }
        Result := (Match(InternalEvaluate(pCanB^.iOperand1, AIsNull),
                         InternalEvaluate(pCanB^.iOperand2, AIsNull)) = -1);
      end;
    canGT:
      begin    { CANBinary; greater than.         (*) }
        Result := (Match(InternalEvaluate(pCanB^.iOperand1, AIsNull),
                         InternalEvaluate(pCanB^.iOperand2, AIsNull)) = 1);
      end;
    canLE:
      begin    { CANBinary; less or equal.        (*) }
        Result := (Match(InternalEvaluate(pCanB^.iOperand1, AIsNull),
                         InternalEvaluate(pCanB^.iOperand2, AIsNull)) <= 0);
      end;
    canGE:
      begin    { CANBinary; greater or equal.     (*) }
        Result := (Match(InternalEvaluate(pCanB^.iOperand1, AIsNull),
                         InternalEvaluate(pCanB^.iOperand2, AIsNull)) >= 0);
      end;
    canEQ:
      begin    { CANBinary, CANCompare; equal.    (*) }
        Result := Match(InternalEvaluate(pCanB^.iOperand1, AIsNull),
                        InternalEvaluate(pCanB^.iOperand2, AIsNull)) = 0;
      end;
    canNE:
      begin    { CANBinary; NOT equal.            (*) }
        Result := Match(InternalEvaluate(pCanB^.iOperand1, AIsNull),
                        InternalEvaluate(pCanB^.iOperand2, AIsNull)) <> 0;
      end;
    canAND:
      begin    { CANBinary; AND                   (*) }
        Result := InternalEvaluate(pCanB^.iOperand1, AIsNull)
                  AND
                  InternalEvaluate(pCanB^.iOperand2, AIsNull);
      end;
    canOR:
      begin    { CANBinary; OR                    (*) }
        Result := InternalEvaluate(pCanB^.iOperand1,AIsNull)
                  OR
                  InternalEvaluate(pCanB^.iOperand2,AIsNull);
      end;
    canADD:
      begin    { CANBinary; addition. }
        Result := InternalEvaluate(pCanB^.iOperand1,AIsNull)
                  +
                  InternalEvaluate(pCanB^.iOperand2,AIsNull);
      end;
    canSUB:
      begin     { CANBinary; subtraction. }
        Result := InternalEvaluate(pCanB^.iOperand1,AIsNull)
                  -
                  InternalEvaluate(pCanB^.iOperand2,AIsNull);
      end;
    canMUL:
      begin     { CANBinary; multiplication. }
        Result := InternalEvaluate(pCanB^.iOperand1,AIsNull)
                  *
                  InternalEvaluate(pCanB^.iOperand2,AIsNull);
      end;
    canDIV:
      begin     { CANBinary; division. }
        Result := InternalEvaluate(pCanB^.iOperand1,AIsNull)
                  /
                  InternalEvaluate(pCanB^.iOperand2,AIsNull);
      end;
    canMOD:
      begin     { CANBinary; modulo division. }
        Result := InternalEvaluate(pCanB^.iOperand1,AIsNull)
                  DIV
                  InternalEvaluate(pCanB^.iOperand2,AIsNull);
      end;
    canREM:
      begin     { CANBinary; remainder of division. }
        Result := InternalEvaluate(pCanB^.iOperand1,AIsNull)
                  MOD
                  InternalEvaluate(pCanB^.iOperand2,AIsNull);
      end;
    canMIN:
      begin     { CANBinary, find minimum of. }
        v1 := InternalEvaluate(pCanB^.iOperand1, AIsNull);
        v2 := InternalEvaluate(pCanB^.iOperand2, AIsNull);
        if v1
           <
           v2 then
          Result := v1
        else
          Result := v2;
      end;
    canMAX:
      begin     { CANBinary, find maximum of. }
        v1 := InternalEvaluate(pCanB^.iOperand1, AIsNull);
        v2 := InternalEvaluate(pCanB^.iOperand2, AIsNull);
        if v1
           >
           v2 then
          Result := v1
        else
          Result := v2;
      end;
    canLIKE:
      begin     { CANCompare, extended binary compare       (*)}
        result := IsLike(InternalEvaluate(pCanB^.iOperand1,AIsNull),
                         InternalEvaluate(pCanB^.iOperand2,AIsNull),
                         False);
      end;
    canIN:
      begin     { CANBinary field in list of values}
        // get pointer to list
        pCanL := pCanListElem(Offset(NodeStart+pCanB^.iOperand2));
        Result := Match(InternalEvaluate(pCanB^.iOperand1, AIsNull),
                        InternalEvaluate(pCanL^.iOffset,   AIsNull)) = 0;
        while not result and (pCanL^.iNextOffset<>0) do begin
          pCanL := pCanListElem(Offset(NodeStart+pCanL^.iNextOffset));
          result := Match(InternalEvaluate(pCanB^.iOperand1, AIsNull),
                          InternalEvaluate(pCanL^.iOffset,   AIsNull)) = 0;
        end;
      end;
    canASSIGN:
      begin     { CANBinary: Field assignment }
        pCanF := pCanField(Offset(NodeStart + pCanB^.iOperand2));
        if (pCanF^.nodeClass <> nodeField) then
          raise EDataBaseError.Create(SExpectingFieldNode);
        fld := GetField(pCanF);
        Result := InternalEvaluate(pCanB^.iOperand1, AIsNull);
        if fld is TLargeintField then
          case VarType(Result) and varTypeMask of
          varSmallint, varInteger:
            fld.AsInteger := Result;
          varSingle, varDouble, varCurrency:
            TLargeintField(fld).AsLargeInt := Round(Result);
          varEmpty, varNull:
            fld.Clear;
          else
            fld.Value := Result;
          end
        else
          fld.Value := Result;
      end;
    else
      raise EDataBaseError.Create(SMissingBinaryFilterEval + IntToStr(Ord(pCanB^.CanOP)));

{   canSUM,                             { CANBinary, accumulate sum of.
    canCOUNT,                           { CANBinary, accumulate count of.
    canAVG,                             { CANBinary, find average of.
    canCONT,                            { CANBinary; provides a link between two
    canUDF2,                            { CANBinary; invokes a User defined fn
    canLIST2,                           { List of constant values of same type
}
    end;
  end;

  function ProcessFieldNode(pCanF: pCanField): Variant;
  var
    fld: TField;
  begin
    fld := GetField(pCanF);
    if fld.IsNull then
      AIsNull := 1
    else
      AIsNull := -1;
    Result := fld.Value;
  end;

  function ProcessConstNode(pCanC: pCanConst): Variant;
  var
    dbl:           Double;
    pDbl:         ^Double;
    pInt:         ^Integer;
    pSmallInt:    ^SmallInt;
    wWord:         Word;
    pWord:        ^Word;
    dDateTime:     TDateTime;
    cCardinal:     Cardinal;
    pCardinal:    ^Cardinal;
    pBCDec:        PBcd;
{$ifdef Delphi6AndUp}
    pTS:           PSQLTimeStamp;
{$else}
    C:             Currency;
{$endif}
  begin
    case TFieldType(pCanC^.iType) of
    ftString:
      Result := GetString(pCanC^.iOffset, pCanC^.iSize);
    ftDate:
      begin
        pCardinal := Offset(pCanC^.iOffset + LiteralStart);
        cCardinal := pCardinal^;
        cCardinal := cCardinal - 693594;
        dDateTime := strtofloat(inttostr(cCardinal));
        Result := dDateTime;
      end;
    ftBoolean:
      begin
        if PChar(Offset(pCanC^.iOffset + LiteralStart))^ = Char(1) then
          Result := True
        else
          Result := False;
      end;
    ftSmallInt:
      begin
        pSmallInt := Offset(pCanC^.iOffset + LiteralStart);
        Result := pSmallInt^;
      end;
    ftInteger,
      ftAutoInc:
      begin
        pInt := Offset(pCanC^.iOffset + LiteralStart);
        Result := pInt^;
      end;
    ftFloat:
      begin
        pDbl := Offset(pCanC^.iOffset + LiteralStart);
        Result := pDbl^;
      end;
    ftTime:
      begin
        pCardinal := Offset(pCanC^.iOffset + LiteralStart);
        cCardinal := pCardinal^;
        Dbl := cCardinal/86400000;
        dDateTime := Dbl;
        Result := dDateTime;
      end;
    ftDateTime:
      begin
        pCardinal := Offset(pCanC^.iOffset + LiteralStart + 1);
        cCardinal := pCardinal^;
        pWord := Offset(pCanC^.iOffset+LiteralStart + 5);
        wWord := pWord^;
        dDateTime := ((wWord * 1.0) - 52032.0) * (4294967296.0/43200000.0) +
                     (((cCardinal * 1.0) - 1568943104.0)/43200000.0);
        Result := dDateTime;
      end;
    ftWord:
      begin
        pWord := Offset(pCanC^.iOffset + LiteralStart);
        Result := pWord^;
      end;
    ftBCD:
      begin
        pBCDec := Offset(pCanC^.iOffset+LiteralStart);
{$ifdef Delphi6AndUp}
        Result := VarFMTBcdCreate(pBCDec^);
{$else}
        BCDToCurr(pBCDec^, C);
        Result := C;
{$endif}
      end;
{$ifdef Delphi6AndUp}
    ftTimeStamp:
      begin
        pTS := Offset(pCanC^.iOffset+LiteralStart);
        Result := VarSQLTimeStampCreate(pTS^);
      end;
{$endif}
    else
      Result := pCanC^.iType;
    end;
  end;

  function ProcessCompareNode(pCanCmp: pCanCompare): Variant;
  var
    v1, v2: Variant;
  begin
    v1 := InternalEvaluate(pCanCmp^.iOperand1, AIsNull);
    v2 := InternalEvaluate(pCanCmp^.iOperand2, AIsNull);
    case pCanCmp^.CanOP of
    canLIKE:
      begin
        Result := IsLike(v1, v2, pCanCmp^.bCaseInsensitive);
      end;
    canNE:
      begin
        Result := StringMatch(v1, v2, pCanCmp^.bCaseInsensitive,
          pCanCmp^.iPartialLen <> 0, pCanCmp^.iPartialLen) <> 0;
      end;
    canEQ:
      begin
        Result := StringMatch(v1, v2, pCanCmp^.bCaseInsensitive,
          pCanCmp^.iPartialLen <> 0, pCanCmp^.iPartialLen) = 0;
      end;
    else
      raise EDataBaseError.Create(SMissingExtendedFilterEval + IntToStr(Ord(pCanCmp^.CanOP)));
    end;
  end;

  function ProcessFuncNode(pCanFnc: pCanFunc): Variant;
  var
    sFuncName: String;
    vArgs:     Variant;
    nArgs:     Integer;
    elemOff:   Integer;
    pCanL:     pCanListElem;
  begin
    case pCanFnc^.CanOP of
    canFUNC2:        { CANFunc: Function }
      begin
        sFuncName := GetString(pCanFnc^.iNameOffset, $FFFF);
        vArgs := null;
        if pCanFnc^.iElemOffset <> 0 then begin
          elemOff := pCanFnc^.iElemOffset;
          nArgs := 0;
          while elemOff <> 0 do begin
            pCanL := pCanListElem(Offset(NodeStart + elemOff));
            elemOff := pCanL^.iNextOffset;
            Inc(nArgs);
          end;
          vArgs := VarArrayCreate([0, nArgs - 1], varVariant);
          nArgs := 0;
          elemOff := pCanFnc^.iElemOffset;
          while elemOff <> 0 do begin
            pCanL := pCanListElem(Offset(NodeStart + elemOff));
            vArgs[nArgs] := InternalEvaluate(pCanL^.iOffset, AIsNull);
            elemOff := pCanL^.iNextOffset;
            Inc(nArgs);
          end;
        end;
        Result := DoFunctionCall(UpperCase(sFuncName), vArgs);
      end;
    else
      raise EDataBaseError.Create(SMissingFunctionalFilterEval + IntToStr(Ord(pCanFnc^.CanOP)));
    end;
  end;

var
  pCanH: pCanHdr;
begin
  AIsNull := 0;
  pCanH := pCanHdr(Offset(NodeStart + AOffset));
  case pCanH^.NodeClass of
  nodeUNARY:   Result := ProcessUnaryNode(pCanUnary(pCanH));
  nodeBINARY:  Result := ProcessBinaryNode(pCanBinary(pCanH));
  nodeField:   Result := ProcessFieldNode(pCanField(pCanH));
  nodeConst:   Result := ProcessConstNode(pCanConst(pCanH));
  nodeCOMPARE: Result := ProcessCompareNode(pCanCompare(pCanH));
  nodeFUNC:    Result := ProcessFuncNode(pCanFunc(pCanH));
  else
    raise EDataBaseError.Create(SMissingnodeTypeFilterEval + IntToStr(Ord(pCanH^.NodeClass)));
  end;
end;

function StrIsNull(const V: Variant): Boolean;
var
  tp: Integer;
begin
  tp := (VarType(V) and varTypeMask);
  Result := (tp = varEmpty) or (tp = varNull) or
    ((tp = varString) or (tp = varOleStr)) and (V = '');
end;

function StrToVar(const S: String): Variant;
begin
  if S = '' then
    Result := Null
  else
    Result := S;
end;

function DoFuncUpper(const AArgs: Variant): Variant;
begin
  if StrIsNull(AArgs[0]) then
    Result := Null
  else
    Result := AnsiUpperCase(AArgs[0]);
end;

function DoFuncLower(const AArgs: Variant): Variant;
begin
  if StrIsNull(AArgs[0]) then
    Result := Null
  else
    Result := AnsiUpperCase(AArgs[0]);
end;

function DoFuncSubstring(const AArgs: Variant): Variant;
var
  s: String;
  ind, cnt: Integer;
begin
  if StrIsNull(AArgs[0]) or StrIsNull(AArgs[1]) then
    Result := Null
  else begin
    s := AArgs[0];
    ind := AArgs[1];
    if ind < 0 then
      ind := Length(s) + ind + 1;
    if VarArrayHighBound(AArgs, 1) = 1 then
      cnt := Length(s)
    else if StrIsNull(AArgs[2]) or (AArgs[2] <= 0) then begin
      Result := Null;
      Exit;
    end
    else
      cnt := AArgs[2];
    Result := StrToVar(Copy(s, ind, cnt));
  end;
end;

type
    TTrimMode = set of (tmLeft, tmRight);

function InternalTrim(const AArgs: Variant; AMode: TTrimMode): Variant;
var
  I, L: Integer;
  sWhere, sWhat: String;
begin
  if StrIsNull(AArgs[0]) then
    Result := Null
  else begin
    sWhere := AArgs[0];
    if VarArrayHighBound(AArgs, 1) = 1 then begin
      if StrIsNull(AArgs[1]) then begin
        Result := Null;
        Exit;
      end
      else
        sWhat := AArgs[1];
    end
    else
      sWhat := ' ';
    L := Length(sWhere);
    I := 1;
    if tmLeft in AMode then
      while (I <= L) and (StrScan(PChar(sWhat), sWhere[I]) <> nil) do
        Inc(I);
    if I > L then
      sWhere := ''
    else begin
      if tmRight in AMode then
        while (L >= I) and (StrScan(PChar(sWhat), sWhere[L]) <> nil) do
          Dec(L);
      sWhere := Copy(sWhere, I, L - I + 1);
    end;
    Result := StrToVar(sWhere);
  end;
end;

function DoFuncTrim(const AArgs: Variant): Variant;
begin
  Result := InternalTrim(AArgs, [tmLeft, tmRight]);
end;

function DoFuncTrimLeft(const AArgs: Variant): Variant;
begin
  Result := InternalTrim(AArgs, [tmLeft]);
end;

function DoFuncTrimRight(const AArgs: Variant): Variant;
begin
  Result := InternalTrim(AArgs, [tmRight]);
end;

function DoFuncYear(const AArgs: Variant): Variant;
var
  Y, M, D: Word;
begin
  if StrIsNull(AArgs[0]) then
    Result := Null
  else begin
    DecodeDate(AArgs[0], Y, M, D);
    Result := Y;
  end;
end;

function DoFuncMonth(const AArgs: Variant): Variant;
var
  Y, M, D: Word;
begin
  if StrIsNull(AArgs[0]) then
    Result := Null
  else begin
    DecodeDate(AArgs[0], Y, M, D);
    Result := M;
  end;
end;

function DoFuncDay(const AArgs: Variant): Variant;
var
  Y, M, D: Word;
begin
  if StrIsNull(AArgs[0]) then
    Result := Null
  else begin
    DecodeDate(AArgs[0], Y, M, D);
    Result := D;
  end;
end;

function DoFuncHour(const AArgs: Variant): Variant;
var
  H, M, S, MS: Word;
begin
  if StrIsNull(AArgs[0]) then
    Result := Null
  else begin
    DecodeTime(AArgs[0], H, M, S, MS);
    Result := H;
  end;
end;

function DoFuncMinute(const AArgs: Variant): Variant;
var
  H, M, S, MS: Word;
begin
  if StrIsNull(AArgs[0]) then
    Result := Null
  else begin
    DecodeTime(AArgs[0], H, M, S, MS);
    Result := M;
  end;
end;

function DoFuncSecond(const AArgs: Variant): Variant;
var
  H, M, S, MS: Word;
begin
  if StrIsNull(AArgs[0]) then
    Result := Null
  else begin
    DecodeTime(AArgs[0], H, M, S, MS);
    Result := S;
  end;
end;

function DoFuncGetDate(const AArgs: Variant): Variant;
begin
  Result := Date;
end;

function DoFuncDate(const AArgs: Variant): Variant;
begin
  if StrIsNull(AArgs[0]) then
    Result := Null
  else
    Result := Integer(Trunc(AArgs[0]));
end;

function DoFuncTime(const AArgs: Variant): Variant;
var
  dt: TDateTime;
begin
  if StrIsNull(AArgs[0]) then
    Result := Null
  else begin
    dt := AArgs[0];
    Result := dt - Trunc(dt);
  end;
end;

function TAstaExpressionTreeReader.DoFunctionCall(const AFunctionName: String; const AArgs: Variant): Variant;
begin
  if AFunctionName = 'UPPER' then
    Result := DoFuncUPPER(AArgs)
  else if AFunctionName = 'LOWER' then
    Result := DoFuncLOWER(AArgs)
  else if AFunctionName = 'SUBSTRING' then
    Result := DoFuncSUBSTRING(AArgs)
  else if AFunctionName = 'TRIM' then
    Result := DoFuncTRIM(AArgs)
  else if AFunctionName = 'TRIMLEFT' then
    Result := DoFuncTRIMLEFT(AArgs)
  else if AFunctionName = 'TRIMRIGHT' then
    Result := DoFuncTRIMRIGHT(AArgs)
  else if AFunctionName = 'YEAR' then
    Result := DoFuncYEAR(AArgs)
  else if AFunctionName = 'MONTH' then
    Result := DoFuncMONTH(AArgs)
  else if AFunctionName = 'DAY' then
    Result := DoFuncDAY(AArgs)
  else if AFunctionName = 'HOUR' then
    Result := DoFuncHOUR(AArgs)
  else if AFunctionName = 'MINUTE' then
    Result := DoFuncMINUTE(AArgs)
  else if AFunctionName = 'SECOND' then
    Result := DoFuncSECOND(AArgs)
  else if AFunctionName = 'GETDATE' then
    Result := DoFuncGETDATE(AArgs)
  else if AFunctionName = 'DATE' then
    Result := DoFuncDATE(AArgs)
  else if AFunctionName = 'TIME' then
    Result := DoFuncTIME(AArgs)
  else
    raise EDataBaseError.Create(SUnknownFunciton + AFunctionName);
end;

end.


