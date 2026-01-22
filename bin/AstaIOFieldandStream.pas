unit AstaIOFieldandStream;
{*********************************************************}
{*                                                       *}
{*     Copyright (c) 1997-2002 Asta Technology Group Inc   *}
{*                 All rights reserved.                  *}
{*                 www.astatech.com                      *}
{*********************************************************}
{$I AstaIO.INC}
interface
uses classes, db, AstaIOCustomDataSet;

function DataSetFieldsToStream(DS: TDataSet): TMemoryStream;
procedure SetDataSetFieldsFromAFieldStream(DS: TDataSet; M: TMemoryStream; SetPositionToZero: Boolean);
procedure MimicFieldProperties(Field, Source: TField; SetIndex: Boolean);
procedure SyncDataSetToPersistantFields(Original, DataSet: TDataSet);
function MimicDataSet(Source: TDataSet): TAstaIOCustomDataset;
function MimicFieldCreate(F: TField; DS: TDataSet): TField;
procedure MimicTableandMakeFieldsPersistant(DS: TDataSet; OpenIt: Boolean);
procedure CreateLookupField(DataSet, LookupDataSet: TDataSet; FieldName, keyfields, LookupKeyfields, LookupresultField: string;
  fieldSize, FieldIndex: Integer);
implementation

//uses dialogs, sysutils, astautil;

function MimicDataSet(Source: TDataSet): TAstaIOCustomDataset;
begin
  result := TAstaIOCustomDataset.Create(nil);
  result.CloneFieldsFromDataSet(source, False, False);
  SyncDataSetToPersistantFields(Source, result);
end;

function DataSetFieldsToStream(DS: TDataSet): TMemoryStream;
var
  i: Integer;
begin
  result := TMemoryStream.Create;
  for i := 0 to ds.fieldcount - 1 do
    result.writecomponent(ds.fields[i]);
end;

procedure MimicFieldProperties(Field, Source: TField; SetIndex: Boolean);
begin
  Field.DisplayLabel := Source.DisplayLabel;
  Field.DisplayWidth := Source.DisplayWidth;
  if SetIndex then Field.Index := Source.Index;
  Field.Tag := Source.Tag;
  Field.ReadOnly := Source.ReadOnly;
  Field.alignment := Source.AlignMent;
  Field.Required := Source.Required;
  Field.Visible := Source.Visible;
{$IFDEF StreamConstraints}
  Field.CustomConstraint := Source.CustomConstraint;
  Field.ConstraintErrorMessage := Source.ConstraintErrorMessage;
  Field.DefaultExpression := Source.DefaultExpression;
  Field.ImportedConstraint := Source.ImportedConstraint;
{$ENDIF}
{$IFDEF ExtendedMimicCode}
  Field.FieldKind := Source.FieldKind;
  if Source.FieldKind = fkLookup then begin
    Field.LookupDataSet := Source.LookupDataSet;
    Field.LookupKeyFields := Source.LookupKeyFields;
    Field.LookupResultField := Source.LookupResultField;
    Field.KeyFields := Source.KeyFields;
    Field.LookupCache := Source.LookupCache;
  end;
{$ENDIF}
  if (Source is TStringField) then
    TStringfield(Field).EditMask := TStringfield(Source).EditMask;
  if (Source is TNumericField) then begin
    TNumericField(Field).DisplayFormat := TNumericField(Source).DisplayFormat;
    TNumericField(Field).EditFormat := TNumericField(Source).EditFormat;
  end;
  if (Source is TIntegerField) then begin
    TIntegerField(Field).MinValue := TIntegerField(Source).MinValue;
    TIntegerField(Field).MaxValue := TIntegerField(Source).MaxValue;
  end;
  if (Source is TFloatField) then begin
    TFloatField(Field).Currency := TFloatField(Source).Currency;
    TFloatField(Field).Precision := TFloatField(Source).Precision;
  end;
  if (Source is TBooleanField) then begin
    TBooleanField(Field).DisplayValues := TBooleanField(Source).DisplayValues;
  end;
  if (Source is TDateTimeField) then begin
    TDateTimeField(Field).DisplayFormat := TDateTimeField(Source).DisplayFormat;
    TDateTimeField(Field).EditMask := TDateTimeField(Source).EditMask;
  end;
end;

procedure SetDataSetFieldsFromAFieldStream(DS: TDataSet; M: TMemoryStream; SetPositionToZero: Boolean);
var
  T: TComponent;
  Field: TField;
begin
  if SetPositionToZero then
    m.position := 0;
  while m.position < m.size do begin
    t := m.readcomponent(nil);
    if (t <> nil) and (t is TField) and (ds.Findfield(Tfield(t).fieldname) <> nil) then begin
      field := ds.findfield(tfield(t).FieldName);
      MimicFieldProperties(Field, TField(t), False);
      t.free;
    end;
  end;
  if ds.active then
    ds.refresh;
end;

procedure SyncDataSetToPersistantFields(Original, DataSet: TDataSet);
var
  i: Integer;
  F: TField;
begin
  for i := 0 to Original.Fieldcount - 1 do begin
    f := DataSet.findfield(Original.Fields[i].FieldName);
    if (f <> nil) and (f.DataType = Original.fields[i].DataType) then
      mimicfieldproperties(f, Original.Fields[i], True);
  end;
end;

function MimicFieldCreate(F: TField; DS: TDataSet): TField;
begin
  result := nil;
  case F.datatype of
    ftstring: result := TStringField.create(ds);
    ftSmallint: result := TSmallintField.Create(ds);
    ftinteger: result := TIntegerField.Create(ds);
    ftWord: result := TWordField.Create(ds);
    ftBoolean: result := TBooleanField.Create(ds);
    ftFloat: result := TFloatField.Create(ds);
    ftCurrency: result := TCurrencyField.Create(ds);
{$IFDEF DELPHI6DATA}
    ftFmtBcd: result := TFMTBCDField.Create(ds);
    ftTimeStamp: result := TSQLTimeStampField.Create(ds);
{$ENDIF}
    ftBCD: result := TBCDField.Create(ds);
    ftDate: result := TDateField.Create(ds);
    ftTime: result := TTimeField.Create(ds);
    ftDateTime: result := TDateTimeField.Create(ds);
    ftBytes: result := TBytesField.Create(ds);
    ftVarBytes: result := TVarBytesField.Create(ds);
    ftAutoInc: result := TAutoIncfield.Create(ds);
    ftBlob: result := TBlobField.Create(ds);
    ftMemo: result := TMemoField.Create(ds);
    ftGraphic: result := TGraphicField.Create(ds);
    ftFmtMemo: result := TBlobField.Create(ds);
    ftParadoxOle: result := TBlobField.Create(ds);
    ftDBaseOle: result := TBlobField.Create(ds);
    ftTypedBinary: result := TBlobField.Create(ds);
//   ftCursor    :
{$IFDEF DELPHI4DATA}
    ftFixedChar: result := TStringField.Create(ds);
    ftWideString: result := TWideStringField.Create(ds);
{$IFDEF DELPHI5DATA}
    ftGUID: result := TGuidField.Create(ds);
{$ENDIF}
    ftLargeint: result := TLargeIntField.Create(ds);
    ftADT: result := TAdtField.Create(ds);
    ftArray: result := TArrayField.Create(ds);
    ftReference: result := TReferenceField.Create(ds);
    ftDataSet: result := TDataSetField.Create(ds);
    ftOraBlob: result := TBlobField.Create(ds);
    ftOraClob: result := TMemoField.Create(ds);
{$ENDIF}
  end;
  if result <> nil then begin
    result.Name := f.Name;
    result.fieldname := f.FieldName;
    result.size := f.size;
    mimicfieldproperties(result, f, True);
    result.DataSet := ds;
    if ds.designer <> nil then
      ds.Designer.DataEvent(deFieldListChange, 0);
  end;
end;

procedure MimicTableandMakeFieldsPersistant(DS: TDataSet; OpenIt: Boolean);
var
  i: Integer;
  CloneDataSet: TAstaIOCustomDataset;
begin
  CloneDataSet := MimicDataSet(DS);
  ds.Close;
  for i := 0 to clonedataset.FieldCount - 1 do
    MimicFieldCreate(clonedataset.fields[i], ds);
  //mimicfieldcreate(t,ds);
  CloneDataSet.Free;
  ds.FieldDefs.Update;
  if OpenIt then
    ds.open;
end;

procedure CreateLookupField(DataSet, LookupDataSet: TDataSet; FieldName, keyfields, LookupKeyfields, LookupresultField: string;
  fieldSize, FieldIndex: Integer);
var
  t: tStringField;
begin
  mimicTableandMakeFieldsPersistant(DataSet, False); //from AstaFieldandStream.pas
  t := TStringField.Create(DataSet);
  t.FieldName := FieldName;
  t.Size := FieldSize;
  t.DataSet := DataSet;
  t.FieldKind := fkLookup;
  t.Lookup := True;
  t.KeyFields := KeyFields;
  t.LookupDataSet := LookupDataSet;
  t.LookupKeyFields := LookupKeyfields;
  t.LookupResultField := LookupResultField;
  t.Index := FieldIndex;
end;
initialization
// In order to use WriteComponent and ReadComponent the Classes must be registered First
  RegisterClasses([TStringField, TNumericField, TIntegerField, TSmallIntField,
    TWordField, TAutoIncField, TFloatField, TCurrencyField, TBCDField, TBooleanField, TDateTimeField,
      TDateField, TTimeField, TBinaryField, TBytesField, TVarBytesField, TBlobField, TMemoField, TGraphicField
{$IFDEF delphi5Data}
    , TWideStringField, TGUIDField, TVariantField, TInterfaceField
      , TIDispatchField, TAggregateField
{$ENDIF}
{$IFDEF delphi4data}
    , TLargeIntField, TDataSetfield, TObjectField
      , TADTField, TArrayField, TReferenceField
{$ENDIF}
{$IFDEF DELPHI6DATA}
    , TFMTBCDField, TSQLTimeStampField
{$ENDIF}
    ]);
end.

