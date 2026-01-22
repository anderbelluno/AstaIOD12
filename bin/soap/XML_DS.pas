unit XML_DS;

{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

{$HINTS OFF}
uses
  DB, Classes, XML_Thunk;

const
  ERR_None = 0;
  ERR_BadDoc = $0E0001; //  first tag xml don't present
  ERR_UnknownFrm = $0E0002; //  no MS or MIDAS sign tag in source
  ERR_SchemaNotDef = $0E0003; //  schema don't defined
  ERR_DataNotDef = $0E0004; //  data rows don't defined
  ERR_FiledsNotDef = $0E0005; //  fileds tag don't defined
  ERR_NoFields = $0E0006; //  empty schema
  ERR_FieldDef = $0E0007; //  error parsing field definition
  ERR_UnknownXML = $0E0008; //  nor ADO nor MIDAS
  ERR_FileNotExist = $0E0009; //  not defined source
  ERR_FileAlreadyExist = $0E000A; //  not defined target
  ERR_MissElem = $0E000B; //  missing requered element
  ERR_MissTypeDef = $0E000C; //  missing type definition
  ERR_FldTypeMap = $0E000D; //  no map for xml field type <> ft...
  ERR_UnexpectedItem = $0E000E; //  unexpected item
  ERR_ExportContent = $0E000F; //  exception while exporting data
  ERR_ImportContent = $0E0010; //  exception while importing data
  ERR_NotImplemented = $0EEEEE; //  not implemented yet

type
  eXMLStyle = (EXMLS_Unknown, EXMLS_ADO, EXMLS_Midas, EXMLS_Qnt);

  rFieldDef = record
    mName: string;
    mType: TFieldType;
    mSize: Integer;
    mRequired: Boolean;
  end;

  funDataSetFactory = function(const dsName: string; const fldDefs: array of rFieldDef): TDataSet of object;

function LoadFromXML(const fileName: string; dsFactory: funDataSetFactory): eXMLStyle; overload;
function LoadFromXML(Doc: TXmlDoc; dsFactory: funDataSetFactory): eXMLStyle; overload;
function DataSetLoadFromXML(D: TDataSet; const Stream: TStream): eXMLStyle; overload;
function DataSetLoadFromXML(D: TDataSet; const FileName: string): eXMLStyle; overload;
procedure DataSetSaveToXML(D: TDataSet; const FileName: string; xmlStyle: eXMLStyle);

type
  funError = procedure(errCode: integer; const info: string = '');

var
  Error: funError = nil;
procedure SaveMIDAS_XML(D: TDataSet; const FileName: string); overload;
procedure SaveMIDAS_XML(D: TDataSet; Stream: TStream); overload;
procedure SaveMS_XML(D: TDataSet; const FileName: string); overload;
procedure SaveMS_XML(D: TDataSet; Stream: TStream); overload;

//======================================================================
implementation uses
  SysUtils,
  TypInfo,
  XSD;

const
  signMS: WideString = 'xml';
  signMIDAS: WideString = 'DATAPACKET';
  signDescrMS: WideString = 's:Schema';
  signDescrMID: WideString = 'METADATA';
  signDataMS: WideString = 'rs:data';
  signDataMID: WideString = 'ROWDATA';
  signFieldsMS: WideString = 's:ElementType';
  signFieldsMID: WideString = 'FIELDS';
  signFldDefMS: WideString = 's:AttributeType';
  signFldDefMID: WideString = 'FIELD';
  signFldType: WideString = 's:datatype';

  attRowName: WideString = 'row';
  attTableName: WideString = 'rs:basetable';
  attNameMS: WideString = 'name';
  attNameMID: WideString = 'fieldname';
  attTypeMS: WideString = 'dt:type';
  attTypeMID: WideString = 'fieldtype';
  attWidthMID: WideString = 'WIDTH';
  attAttrName: WideString = 'attrname';
  attNullable: WideString = 'rs:nullable';
  signRowDataMS: WideString = 'z:row';
  signRowDataMID: WideString = 'ROW';

type
  TFactoryStub = class(TObject)
  private
    mDS: TDataSet;
  public
    constructor Create(ds: TDataSet);
    function Factory(const dsName: string;
      const fldDefs: array of rFieldDef): TDataSet;
  end;

constructor TFactoryStub.
  Create(ds: TDataSet);
begin
  mDS := ds;
end;

function TFactoryStub.Factory(const dsName: string;
  const fldDefs: array of rFieldDef): TDataSet;
var
  i: integer;
begin
    //  dsName  ?
  mDS.Close;
  mDS.FieldDefs.Clear;

  for i := Low(fldDefs) to High(fldDefs) do
    with mDS.FieldDefs.AddFieldDef, fldDefs[i] do
    begin
      Name := mName;
      DataType := mType;
      Size := mSize;
    end;

  Result := mDS;
end;

procedure ErrorStub(errCode: integer; const msg: string);
var
  s: string;
begin
  s := 'XML data error ' + IntToHex(errCode, 6);
  if msg <> '' then s := s + ': ' + msg;
  raise Exception.Create(s);
end;

function DataSetLoadFromXML(D: TDataSet; const FileName: string): eXMLStyle;
var
  dsFactory: TFactoryStub;
begin
  dsFactory := TFactoryStub.Create(D);
  try
    Result := LoadFromXML(FileName, dsFactory.Factory);
  finally
    dsFactory.Free;
  end;
end;

function DataSetLoadFromXML(D: TDataSet; const Stream: TStream): eXMLStyle;
var
  dsFactory: TFactoryStub;
begin
  dsFactory := TFactoryStub.Create(D);
  try
  result:=EXMLS_Unknown;
  //    Result := LoadFromXML(Stream, dsFactory.Factory);
  finally
    dsFactory.Free;
  end;
end;

procedure ImportContent(ds: TDataSet; data: TdomElement;
  const srcNames: array of string; const signRowData: string);
var
  i: integer;
  row: TdomElement;
begin
  try
    ds.Open;

    if not AXML_FindElement(data, signRowData, row) then
      Error(ERR_DataNotDef);

    repeat
      ds.Append;
      for i := 0 to ds.FieldCount - 1 do
        SetFldVal(ds.Fields[i], AXML_AttrAsStr(row, srcNames[i]));
      try
        ds.Post;
      except;
      end;
    until not AXML_NextElement(row, signRowData, row)
  except on E: Exception do
      Error(ERR_ImportContent, E.Message);
  end;
end;

function GetInt(node: TdomElement; const attr: string): integer;
var
  s: string;
begin
  s := AXML_AttrAsStr(node, attr);
  if (s <> '') then
    Result := StrToInt(s)
  else
    Result := 0;
end;

function FldDes(node: TdomElement; var def: rFieldDef; var attName: string): boolean;

var
  t: string;
  name: string;
  fldType: string;
  subtype: string;
//      n, v      : string;
  readonly: boolean;
  required: boolean;
  maxlength: integer;
//      decimals  : integer;
//      fixeddec  : integer;
  valtype: TFieldType;
//      minval    : Int64;
//      maxval    : Int64;
//      i         : integer;
//      p         : TdomElement;
begin

  t := AXML_AttrAsStr(node, attNameMID);
  attName := AXML_AttrAsStr(node, attAttrName);
  if attName = '' then
    attName := AXML_AttrAsStr(node, 'tagname');

  if t = '' then
    name := attName
  else
    name := t;

  fldType := AXML_AttrAsStr(node, attTypeMID);
  valtype := ftUnknown;
  readonly := (AXML_AttrAsStr(node, 'readonly') <> '');
  required := (AXML_AttrAsStr(node, 'required') <> '') and not readonly;
  maxlength := GetInt(node, attWidthMID);
  subtype := AXML_AttrAsStr(node, 'SUBTYPE');

  if (subtype = 'Text') then
    maxlength := 0;

  if (fldType = 'bin.hex') then
  begin
    if subtype = 'Text' then
      valtype := ftMemo
    else
      if (subtype = 'TypedBinary')
      or (subtype = 'Graphics') then
      valtype := ftTypedBinary
    else
      valtype := ftBlob;
  end
  else
    if (fldType = 'i1') then
    valtype := ftInteger
  else
    if (fldType = 'i2') then
    valtype := ftInteger
  else
    if (fldType = 'i4') then
    valtype := ftInteger
  else
    if (fldType = 'date') then
    valtype := ftDate
  else
    if (fldType = 'dateTime') then
    valtype := ftDateTime
  else
    if (fldType = 'time') then
    valtype := ftTime
  else
    if (fldType = 'boolean') then
    valtype := ftBoolean
  else
    if (fldType = 'string')
    or (fldType = 'string.uni') then
    valtype := ftString
  else
    if (fldType = 'r8')
    or (fldType = 'fixed')
    or (fldType = 'number') then
    valtype := ftFloat;

  def.mName := name;
  def.mType := valtype;
  def.mSize := maxlength;
  def.mRequired := required;
  Result := (def.mType <> ftUnknown);
end;

procedure LoadMIDAS_XML(xml: TdomElement; dsFactory: funDataSetFactory);
var
  sch: TdomElement;
  data: TdomElement;
  fields: TdomElement;
  fld: TdomElement;
  fldCnt: integer;
  ds: TDataSet;
  defs: array of rFieldDef;
  srcNames: array of string;
  dsName: string;
begin
  if not AXML_FindElement(xml, signDescrMID, sch) then
    Error(ERR_SchemaNotDef, xml.NodeName);

  if not AXML_FindElement(xml, signDataMID, data) then
    Error(ERR_DataNotDef, xml.NodeName);

  if not AXML_FindElement(sch, signFieldsMID, fields) then
    Error(ERR_FiledsNotDef, xml.NodeName);

  if not AXML_FindElement(fields, signFldDefMID, fld) then
    Error(ERR_NoFields, xml.NodeName);

  fldCnt := 0;

  repeat
    inc(fldCnt);
  until not AXML_NextElement(fld, signFldDefMID, fld);

  SetLength(srcNames, fldCnt);
  SetLength(defs, fldCnt);

  fldCnt := 0;
  dsName := '';

  AXML_FindElement(fields, signFldDefMID, fld);
  repeat
    if FldDes(fld, defs[fldCnt], srcNames[fldCnt]) then
      inc(fldCnt)
    else
      Error(ERR_FieldDef, 'field no ' + IntToStr(fldCnt + 1));
  until not AXML_NextElement(fld, signFldDefMID, fld);

  if dsName = '' then
    dsName := 'Unknown';

  ds := dsFactory(dsName, defs);
  ImportContent(ds, data, srcNames, signRowDataMID);
end;

procedure LoadMS_XML(xml: TdomElement; dsFactory: funDataSetFactory);
var
  sch: TdomElement;
  data: TdomElement;
  fields: TdomElement;
  dtType: TdomElement;
  fld: TdomElement;
  fldCnt: integer;
  ds: TDataSet;
  srcNames: array of string;
  defs: array of rFieldDef;
  fldType: TFieldType;
  fldSize: integer;
  s: string;
  dsName: string;
  nameAttr: string;
  typeAttr: string;
  precAttr: string;
  sizeAttr: string;
begin
  if not AXML_FindElement(xml, signDescrMS, sch) then
    Error(ERR_SchemaNotDef, xml.NodeName);

  if not AXML_FindElement(xml, signDataMS, data) then
    Error(ERR_DataNotDef, xml.NodeName);

  if not AXML_FindElement(sch, signFieldsMS, fields) then
    Error(ERR_FiledsNotDef, xml.NodeName);

  dsName := '';

  if AXML_AttrAsStr(fields, attNameMS) = attRowName then
  begin
    if AXML_FindElement(fields, signFldDefMS, fld) then
    begin
      fldCnt := 0;
      repeat
        inc(fldCnt);
      until not AXML_NextElement(fld, signFldDefMS, fld);

      SetLength(srcNames, fldCnt);
      SetLength(defs, fldCnt);
      fldCnt := 0;

      AXML_FindElement(fields, signFldDefMS, fld);
      repeat
        if dsName = '' then
          dsName := AXML_AttrAsStr(fld, attTableName);

        nameAttr := AXML_AttrAsStr(fld, attNameMS);
        s := AXML_AttrAsStr(fld, attAttrName);
        if s <> '' then
          srcNames[fldCnt] := s
        else
          srcNames[fldCnt] := nameAttr;

        if AXML_FindElement(fld, signFldType, dtType) then
        begin
          typeAttr := AXML_AttrAsStr(dtType, attTypeMS);

          if not SchemaTypeToFldType(typeAttr, fldType, precAttr, sizeAttr) then
            Error(ERR_FldTypeMap, typeAttr);

          fldSize := 0;
          if sizeAttr <> '' then
          begin
            s := AXML_AttrAsStr(dtType, sizeAttr);
            if s <> '' then fldSize := StrToInt(s);
          end;

          s := AXML_AttrAsStr(fld, attNullable);
          with defs[fldCnt] do
          begin
            mName := nameAttr;
            mType := fldType;
            mSize := fldSize;
            mRequired := (s = '') or (s <> 'true');
            if (mType = ftString) and (mSize > dsMaxStringSize) then
            begin
              mSize := 0;
              mType := ftMemo;
            end;
          end;
        end
        else
          Error(ERR_MissTypeDef, s);

        inc(fldCnt);
      until not AXML_NextElement(fld, signFldDefMS, fld);

      if dsName = '' then
        dsName := 'Unknown';

      ds := dsFactory(dsName, defs);
      ImportContent(ds, data, srcNames, signRowDataMS);
    end
    else
      Error(ERR_NoFields, xml.NodeName);
  end
  else
    Error(ERR_UnexpectedItem, AXML_AttrAsStr(fields, attNameMS) + ' (need ' + attRowName + ')');
end;

function LoadFromXML(const fileName: string; dsFactory: funDataSetFactory): eXMLStyle;
var
  doc: TXMLDoc;
  xml: TdomElement;
  defs: array of rFieldDef;
begin
  Result := EXMLS_Unknown;
  doc := nil;
  xml := nil;
  defs := nil;

  if not FileExists(fileName) then
    Error(ERR_FileNotExist, fileName);

  try
    doc := AXML_LoadFile(fileName);

    if AXML_FindElement(doc.documentElement, signMS, xml, false) then
    begin
      LoadMS_XML(xml, dsFactory);
      Result := EXMLS_ADO;
    end
    else
      if AXML_FindElement(doc.documentElement, signMIDAS, xml, false) then
    begin
      LoadMIDAS_XML(xml, dsFactory);
      Result := EXMLS_Midas;
    end
    else
      Error(ERR_UnknownFrm, fileName);
  finally
    xml := nil;
    AXML_ReleaseDoc(doc);
  end;
end;

function LoadFromXML(Doc: TXmlDoc; dsFactory: funDataSetFactory): eXMLStyle;
var
  xml: TdomElement;
begin
  Result := EXMLS_Unknown;

  if AXML_FindElement(doc.documentElement, signMS, xml, false) then
  begin
    LoadMS_XML(xml, dsFactory);
    Result := EXMLS_ADO;
  end
  else
    if AXML_FindElement(doc.documentElement, signMIDAS, xml, false) then
  begin
    LoadMIDAS_XML(xml, dsFactory);
    Result := EXMLS_Midas;
  end
  else
    raise Exception.Create('Could not load XML');
end;

function FieldTypeAsStr(fldType: TFieldType): string;
begin
  Result := GetEnumName(TypeInfo(TFieldType), ord(fldType));
end;

function FldAsAttr(i: integer): string;
begin
  Result := 'Attribute_' + IntToStr(i + 1);
end;

procedure ExportContent(D: TDataSet; defs: array of rFieldDef; doc: TXMLDoc;
  data: TdomElement; signRow: WideString);
var
  i: integer;
  row: TdomElement;
begin
  try
    D.Open;
    D.First;
    while not D.Eof do
    begin
      row := AXML_CreateDocElem(signRow, doc, data);

      for i := 0 to D.FieldCount - 1 do
        row.setAttribute(FldAsAttr(i), GetFldVal(D.Fields[i],
          defs[i].mType));

      D.Next;
    end;
  except on E: Exception do
      Error(ERR_ExportContent, E.Message);
  end;
end;

procedure SaveMS_XML(D: TDataSet; const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveMS_XML(D, Stream);
  finally
    Stream.Free;
  end;
end;

procedure SaveMS_XML(D: TDataSet; Stream: TStream);
var
  doc: TXMLDoc;
  xml: TdomElement;
  sch: TdomElement;
  data: TdomElement;
  fields: TdomElement;
  fld: TdomElement;
  dtType: TdomElement;
  i: integer;
  defs: array of rFieldDef;
  fldDef: TFieldDef;
  fldSize: integer;
  typeAttr: string;
  precAttr: string;
  sizeAttr: string;
begin
  xml := nil;
  sch := nil;
  data := nil;
  fields := nil;
  fld := nil;
  dtType := nil;
  doc := AXML_CreateTarget(signMS, false, 'utf-8', xml);
  doc.CurrEnc:=$FF07;//enUTF8
  try
    xml.setAttribute('xmlns:s', 'uuid:BDC6E3F0-6DA3-11d1-A2A3-00AA00C14882');
    xml.setAttribute('xmlns:dt', 'uuid:C2F41010-65B3-11d1-A29F-00AA00C14882');
    xml.setAttribute('xmlns:rs', 'urn:schemas-microsoft-com:rowset');
    xml.setAttribute('xmlns:z', '#RowsetSchema');

    sch := AXML_CreateDocElem(signDescrMS, doc, xml);
    sch.setAttribute('id', 'RowsetSchema');

    data := AXML_CreateDocElem(signDataMS, doc, xml);

    fields := AXML_CreateDocElem(signFieldsMS, doc, sch);
    fields.setAttribute(attNameMS, attRowName);
    fields.setAttribute('content', 'eltOnly'); //?
    fields.setAttribute('rs:updatable', 'true'); //?

    SetLength(defs, D.FieldDefs.Count);

    for i := 0 to D.FieldDefs.Count - 1 do
    begin
      fld := AXML_CreateDocElem(signFldDefMS, doc, fields);

      fldDef := D.FieldDefs[i];
      fld.setAttribute(attAttrName, FldAsAttr(i));
      fld.setAttribute(attNameMS, fldDef.Name);
      fld.setAttribute('rs:number', IntToStr(fldDef.FieldNo)); //?
      if fldDef.Required then
        fld.setAttribute(attNullable, 'false')
      else
        fld.setAttribute(attNullable, 'true');

      fldSize := fldDef.Size;

      defs[i].mType := fldDef.DataType;
        if fldDef.DataType = ftMemo then
        begin
          defs[i].mType := ftString;
          typeAttr := 'string';
          sizeAttr := 'dt:maxLength';
          fldSize := 536870910;
        end
        else
          if (fldDef.DataType = ftTypedBinary) then
        begin
          defs[i].mType := ftBlob;
          typeAttr := 'bin.hex';
          sizeAttr := 'dt:maxLength';
          fldSize := 1073741823;
        end
        else
      if not FldTypeToSchemaType(fldDef.DataType, typeAttr, precAttr, sizeAttr) then
          Error(ERR_FldTypeMap, FieldTypeAsStr(fldDef.DataType));

      dtType := AXML_CreateDocElem(signFldType, doc, fld);
      dtType.setAttribute(attTypeMS, typeAttr);

      if precAttr <> '' then
        dtType.setAttribute(precAttr, IntToStr(fldDef.Precision));
      if sizeAttr <> '' then
        dtType.setAttribute(sizeAttr, IntToStr(fldSize));
    end;

    ExportContent(D, defs, doc, data, signRowDataMS);

    AXML_SaveStream(Stream, doc);
    //AXML_SaveTarget(FileName, doc);
  finally
    xml := nil;
    sch := nil;
    data := nil;
    fields := nil;
    fld := nil;
    dtType := nil;
    AXML_ReleaseDoc(doc);
  end;
end;

procedure SaveMIDAS_XML(D: TDataSet; const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveMidas_XML(D, Stream);
  finally
    Stream.Free;
  end;
end;

procedure SaveMIDAS_XML(D: TDataSet; Stream: TStream);
var
  doc: TXMLDoc;
  xml: TdomElement;
  sch: TdomElement;
  data: TdomElement;
  fields: TdomElement;
  fld: TdomElement;
  i: integer;
  defs: array of rFieldDef;
  fldDef: TFieldDef;
  fldSize: integer;
  typeAttr: string;
  precAttr: string;
  sizeAttr: string;
begin
  xml := nil;
  sch := nil;
  data := nil;
  fields := nil;
  fld := nil;
  doc := AXML_CreateTarget(signMIDAS, true, 'utf-8', xml);
  doc.CurrEnc:=$FF07;//enUTF8
  try
    xml.setAttribute('Version', '2.0');

    sch := AXML_CreateDocElem(signDescrMID, doc, xml);
    data := AXML_CreateDocElem(signDataMID, doc, xml);
    fields := AXML_CreateDocElem(signFieldsMID, doc, sch);

    SetLength(defs, D.FieldDefs.Count);

    for i := 0 to D.FieldDefs.Count - 1 do
    begin
      fld := AXML_CreateDocElem(signFldDefMID, doc, fields);

      fldDef := D.FieldDefs[i];
      fld.setAttribute(attAttrName, FldAsAttr(i));
      fld.setAttribute(attNameMID, fldDef.Name);

      fldSize := fldDef.Size;

      defs[i].mType := fldDef.DataType;
      if not FldTypeToSchemaType(fldDef.DataType, typeAttr, precAttr, sizeAttr) then
        if fldDef.DataType = ftTypedBinary then
        begin
          typeAttr := 'bin.hex';
          fld.setAttribute('SUBTYPE', 'TypedBinary')
        end
        else
          Error(ERR_FldTypeMap, FieldTypeAsStr(fldDef.DataType));

      fld.setAttribute(attTypeMID, typeAttr);
      if fldSize <> 0 then
        fld.setAttribute(attWidthMID, IntToStr(fldSize));
    end;

    ExportContent(D, defs, doc, data, signRowDataMID);

    AXML_SaveStream(Stream, doc);
//    AXML_SaveTarget(FileName, doc);
  finally
    xml := nil;
    sch := nil;
    data := nil;
    fields := nil;
    fld := nil;
    AXML_ReleaseDoc(doc);
  end;
end;

procedure DataSetSaveToXML(D: TDataSet; const FileName: string; xmlStyle: eXMLStyle);
begin
  if FileExists(FileName) then
    raise Exception.Create('File ' + FileName + ' already exist');

  case xmlStyle of
    EXMLS_ADO: SaveMS_XML(D, FileName);
    EXMLS_Midas: SaveMidas_XML(D, FileName);
  else
    Error(ERR_UnknownXML, Format('xmlStyle %d', [ord(xmlStyle)]));
  end;
end;
{$T-)
initialization
  Error := @ErrorStub;
{$HINTS ON}
end.

