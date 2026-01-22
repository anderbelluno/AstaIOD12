{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10171: AstaIOIndexes.pas 
{
{   Rev 1.0    4/10/2003 6:31:04 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:22 PM  Steve    Version: 1.505
}
unit AstaIOIndexes;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

Uses SysUtils, Classes, DB, AstaIODBList;

type
  TAstaIOIndex = class;
  TAstaIOIndexes = class;

  TAstaIOIndexOption = (ioPrimary, ioUnique, ioDescending, ioCaseInsensitive,
    ioNonMaintained, ioNatural);
  TAstaIOIndexOptions = set of TAstaIOIndexOption;

  TAstaIOIndexStatus = (isActual, isUpdating, isHasCalcs);
  TAstaIOIndexStatuses = set of TAstaIOIndexStatus;

  TAstaIOLocateOption = (loNearest, loFiltered, loEnd, loExclRec);
  TAstaIOLocateOptions = set of TAstaIOLocateOption;

  TAstaIOIndexCompareRecOption = (crDescending, crPartial, crCaseInsensitive,
    crNearest, crUseBookmark, crFiltered, crEnd, crExclRec);
  TAstaIOIndexCompareRecOptions = set of TAstaIOIndexCompareRecOption;

  TAstaIOIndexReason = (irNone, irDefinition, irMaintenance, irSelected, irDeleted);
  TAstaIOIndexChangeEvent = procedure (AIndex: TAstaIOIndex; AReason: TAstaIOIndexReason) of object;

  TAstaIOIndex = class(TCollectionItem)
  private
    FRecs: TList;
    FRecsOwned: Boolean;
    FName: String;
    FFields: String;
    FDescendings: String;
    FCaseInsensitives: String;
    FFieldList: TStrings;
    FOptions: TAstaIOIndexOptions;
    FStatuses: TAstaIOIndexStatuses;
    FUpdatingRecordIndex: Integer;
    FUpdateCount: Integer;
    function GetByBookmark: Boolean;
    procedure SetByBookmark(AValue: Boolean);
    procedure SetFields(const Value: String);
    procedure SetName(const Value: String);
    procedure SetOptions(const Value: TAstaIOIndexOptions);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetCount: Integer;
    function GetRecords(AIndex: Integer): TAstaDBListItem;
    function GetDataSet: TDataSet;
    function GetActual: Boolean;
    function GetUnique: Boolean;
    function GetSelected: Boolean;
    procedure SetSelected(const Value: Boolean);
    function GetFieldCount: Integer;
    function GetFieldNames(AIndex: Integer): String;
    function GetFieldObj(AIndex: Integer): TField;
    procedure UpdateFieldList;
    procedure SetDescendings(const Value: String);
    procedure SetCaseInsensitives(const Value: String);
  protected
    function GetDisplayName: string; override;
    procedure Changed(AReason: TAstaIOIndexReason);
    procedure Changing(AReason: TAstaIOIndexReason);
  public
    // ***** this public part for TDataSet's usage only *****
    // data editing
    function RecordUpdating(ARec: TAstaDBListItem): Integer;
    procedure RecordCheckUpdate(AField: TField);
    function RecordUpdated(ARec: TAstaDBListItem): Integer;
    function RecordInserting(ARec: TAstaDBListItem): Integer;
    function RecordInserted(ARec: TAstaDBListItem): Integer;
    function RecordDeleted(ARec: TAstaDBListItem): Integer;
    function RecordCancel(ARec: TAstaDBListItem): Integer;
    // locate
    function CompareRecs(ARec1, ARec2: TAstaDBListItem; AMatchingFields: Integer;
      AOptions: TAstaIOIndexCompareRecOptions): Integer;
    function LocateRecord(ARec: TAstaDBListItem; AMatchingFields: Integer;
      AAstaOptions: TAstaIOLocateOptions; AOptions: TLocateOptions;
      var APosition: Integer): Integer;
    function LocateByBookmark(ABmk: Integer; AAstaOptions: TAstaIOLocateOptions;
      AOptions: TLocateOptions; var APosition: Integer): Integer;
    // records
    property RecsCount: Integer read GetCount;
    property DataSet: TDataSet read GetDataSet;
    property Recs[AIndex: Integer]: TAstaDBListItem read GetRecords;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(AValue: TPersistent); override;
    // maintenance
    procedure Clear;
    procedure AddRecords;
    procedure Sort;
    procedure Rebuild;
    procedure UpToDate;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure FieldsDefined;
    procedure FieldsUnDefined;
    procedure DBListCreated;
    procedure DBListDestroyed;
    // read & write
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    // attributes
    property ByBookmark: Boolean read GetByBookmark write SetByBookmark;
    property Actual: Boolean read GetActual;
    property Unique: Boolean read GetUnique;
    property FieldCount: Integer read GetFieldCount;
    property FieldNames[AIndex: Integer]: String read GetFieldNames;
    property FieldObj[AIndex: Integer]: TField read GetFieldObj;
    property Descendings: String read FDescendings write SetDescendings;
    property CaseInsensitives: String read FCaseInsensitives write SetCaseInsensitives;
  published
    property Name: String read FName write SetName;
    property Fields: String read FFields write SetFields;
    property Options: TAstaIOIndexOptions read FOptions write SetOptions default [ioNonMaintained];
    property Active: Boolean read GetActive write SetActive stored False;
    property Selected: Boolean read GetSelected write SetSelected default False;
  end;

  TAstaIOIndexes = class(TCollection)
  private
    FDataSet: TDataSet;
    FSelectedIndex: Integer;
    FPosition: Integer;
    FActive: Boolean;
    FBMKActive: Boolean;
    FBmk: Integer;
    FOnChanging: TAstaIOIndexChangeEvent;
    FOnChanged: TAstaIOIndexChangeEvent;
    function GetIndex(AIndex: Integer): TAstaIOIndex;
    function GetSelectedIndexFields: String;
    function GetSelectedIndexName: String;
    procedure SetSelectedIndexFields(const Value: String);
    procedure SetSelectedIndexName(const Value: String);
    function GetSelectedIndex: TAstaIOIndex;
    function GetBookmark: Integer;
    function GetRec: TAstaDBListItem;
    function GetRecsCount: Integer;
    procedure SetBookmark(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    function GetOrdered: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetSelectedIndexNo(const Value: Integer; AForce: Boolean);
    function GetIsPositionValid: Boolean;
  protected
    function GetOwner: TPersistent; override;
    procedure IndexChanging(AIndex: TAstaIOIndex; AReason: TAstaIOIndexReason);
    procedure IndexChanged(AIndex: TAstaIOIndex; AReason: TAstaIOIndexReason);
  public
    // ***** this public part for TDataSet's usage only *****
    // data editing
    procedure RecordUpdating(ARec: TAstaDBListItem);
    procedure RecordCheckUpdate(AField: TField);
    procedure RecordUpdated(ARec: TAstaDBListItem);
    procedure RecordInserting(ARec: TAstaDBListItem);
    procedure RecordInserted(ARec: TAstaDBListItem);
    procedure RecordDeleted(ARec: TAstaDBListItem);
    procedure RecordCancel(ARec: TAstaDBListItem);
    // locate
    function LocateRecord(ARec: TAstaDBListItem; const AFields: String;
      AAstaOptions: TAstaIOLocateOptions; AOptions: TLocateOptions; var APosition: Integer): Integer;
    function LocateByBookmark(ABmk: Integer; AAstaOptions: TAstaIOLocateOptions;
      AOptions: TLocateOptions; var APosition: Integer): Integer;
    // navigation
    property Bookmark: Integer read GetBookmark write SetBookmark;
    property Position: Integer read FPosition write SetPosition;
    property RecsCount: Integer read GetRecsCount;
    property Rec: TAstaDBListItem read GetRec;
    property IsPositionValid: Boolean read GetIsPositionValid;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TAstaIOIndex;
    function FindIndex(const AName: String): TAstaIOIndex;
    function IndexByName(const AName: String): TAstaIOIndex;
    function IndexSure(const AName: String): TAstaIOIndex;
    function IndexForFields(const AFields: String;
      AOptions: TAstaIOIndexOptions; var AMatchingFields: Integer;
      AActive: Integer): TAstaIOIndex;
    function IndexForBookmark: TAstaIOIndex;
    // maintenance
    procedure Rebuild;
    procedure UpToDate;
    procedure FieldsDefined;
    procedure FieldsUnDefined;
    procedure DBListCreated;
    procedure DBListDestroyed;
    // read & write
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    // collection
    property Items[AIndex: Integer]: TAstaIOIndex read GetIndex; default;
    property DataSet: TDataSet read FDataSet;
    // get/set active index
    property SelectedIndexName: String read GetSelectedIndexName write SetSelectedIndexName;
    property SelectedIndexFields: String read GetSelectedIndexFields write SetSelectedIndexFields;
    property SelectedIndex: TAstaIOIndex read GetSelectedIndex;
    // attributes
    property Ordered: Boolean read GetOrdered;
    property Active: Boolean read FActive write SetActive;
    property BMKActive: Boolean read FBMKActive write FBMKActive;
    property OnChanging: TAstaIOIndexChangeEvent read FOnChanging write FOnChanging;
    property OnChanged: TAstaIOIndexChangeEvent read FOnChanged write FOnChanged;
  end;

  TAstaIOIndexDefs = class(TIndexDefs)
  private
    FIndexes: TAstaIOIndexes;
    FSelfUpdating: Boolean;
    procedure DoUpdate(ASender: TObject);
  public
    constructor Create(ADataSet: TDataSet; AIndexes: TAstaIOIndexes);
    procedure Defs2Indexes;
    procedure Indexes2Defs;
  end;

  TAstaIOAggregateValue = class
  private
    FValue: Variant;
    FUsageCount: Integer;
  public
    procedure Attach;
    procedure Detach;
    property Value: Variant read FValue write FValue;
  end;

  TAstaIOAggKind = (akSum, akAvg, akCount, akMin, akMax);
  TAstaIOAggStates = set of (asActual, asUpdating);

  TAstaIOAggregate = class(TCollectionItem)
  private
    FName: String;
    FFieldName: String;
    FAggKind: TAstaIOAggKind;
    FIndexName: String;
    FLevel: Integer;
    FActive: Boolean;
    FValueIndex: Integer;
    FCountIndex: Integer;
    FState: TAstaIOAggStates;
    FUpdatingRec: TastaDBListitem;
    FUpdatingVal: Variant;
    FAggField: TField;
    function GetActual: Boolean;
    function GetRunning: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetAggKind(const Value: TAstaIOAggKind);
    procedure SetFieldName(const Value: String);
    procedure SetIndexName(const Value: String);
    procedure SetLevel(const Value: Integer);
    procedure UpToDate;
    function GetValue: Variant;
    function GetName: String;
    procedure SetName(const Value: String);
    function BuildName: String;
    procedure AllocAggVals(var AValObj, ACntObj: TAstaIOAggregateValue);
    procedure AttachAggVals(ARec: TastaDBListitem; AValObj, ACntObj: TAstaIOAggregateValue);
    procedure DetachAggVals(ARec: TastaDBListitem);
    procedure IncAggVals(AVal: Variant; AValObj, ACntObj: TAstaIOAggregateValue);
    procedure DecAggVals(AVal: Variant; AValObj, ACntObj: TAstaIOAggregateValue);
    procedure GetAggVals(ARec: TastaDBListitem; var AValObj, ACntObj: TAstaIOAggregateValue);
    function GetDataSet: TDataSet;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(AValue: TPersistent); override;
    // read & write
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    // data editing
    procedure RecordUpdating(ARec: TAstaDBListItem);
    procedure RecordCheckUpdate(AField: TField);
    procedure RecordUpdated(ARec: TAstaDBListItem);
    procedure RecordInserted(ARec: TAstaDBListItem);
    procedure RecordDeleted(ARec: TAstaDBListItem);
    procedure RecordCancel(ARec: TAstaDBListItem);
    // maintenance
    procedure Clear;
    procedure Calculate;
    procedure Recalc;
    procedure FieldsDefined;
    procedure FieldsUnDefined;
    // attributes
    property Running: Boolean read GetRunning;
    property Actual: Boolean read GetActual;
    property DataSet: TDataSet read GetDataSet;
    // value
    property Value: Variant read GetValue;
  published
    property Name: String read GetName write SetName;
    property FieldName: String read FFieldName write SetFieldName;
    property AggKind: TAstaIOAggKind read FAggKind write SetAggKind default akCount;
    property IndexName: String read FIndexName write SetIndexName;
    property Level: Integer read FLevel write SetLevel default 0;
    property Active: Boolean read FActive write SetActive default False;
  end;

  TAstaIOAggregates = class(TCollection)
  private
    FValuesPool: TBits;
    FActive: Boolean;
    FIndexes: TAstaIOIndexes;
    FRecs: TAstaDBList;
    FAggCellsAllocated: Integer;
    procedure SetActive(const Value: Boolean);
    function GetItems(AIndex: Integer): TAstaIOAggregate;
    procedure DoRecAdded(ARec: TAstaDBListItem);
    procedure DoRecDeleted(ARec: TAstaDBListItem);
  protected
    function GetOwner: TPersistent; override;
    procedure DeleteValues(var AIndex: Integer);
    function AllocValues: Integer;
  public
    constructor Create(AIndexes: TAstaIOIndexes; ARecs: TAstaDBList);
    destructor Destroy; override;
    function Add: TAstaIOAggregate;
    function FindAggregate(const AName: String): TAstaIOAggregate;
    function AggregateByName(const AName: String): TAstaIOAggregate;
    // read & write
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    // data editing
    procedure RecordUpdating(ARec: TAstaDBListItem);
    procedure RecordCheckUpdate(AField: TField);
    procedure RecordUpdated(ARec: TAstaDBListItem);
    procedure RecordInserted(ARec: TAstaDBListItem);
    procedure RecordDeleted(ARec: TAstaDBListItem);
    procedure RecordCancel(ARec: TAstaDBListItem);
    // maintenence
    procedure Recalc;
    procedure RecalcOnIndex(const AIndexName: String);
    procedure UpToDate;
    procedure FieldsDefined;
    procedure FieldsUnDefined;
    procedure DBListCreated;
    property Active: Boolean read FActive write SetActive default True;
    property Items[AIndex: Integer]: TAstaIOAggregate read GetItems; default;
  end;

const
  SASTADefIndexName = '__ASTAIO__DEFAULT';

implementation

Uses AstaIOResources, AstaIOCustomDataSet
{$ifdef Delphi6AndUp}
    , Variants, FMTBcd
{$endif}
    ;

type
  __TAstaIOCustomDataSet = class(TAstaIOCustomDataSet);

procedure WriteEnum(AStream: TStream; const E; ALen: Integer);
var
  i: Integer;
begin
  i := 0;
  Move(E, i, ALen);
  AStream.WriteBuffer(i, SizeOf(i));
end;

procedure ReadEnum(AStream: TStream; var E; ALen: Integer);
var
  i: Integer;
begin
  AStream.ReadBuffer(i, SizeOf(i));
  Move(i, E, ALen);
end;

procedure WriteString(AStream: TStream; const S: String);
var
  i: Integer;
begin
  i := Length(S);
  AStream.WriteBuffer(i, SizeOf(i));
  if i > 0 then
    AStream.WriteBuffer(S[1], i);
end;

function ReadString(AStream: TStream): String;
var
  i: Integer;
begin
  AStream.ReadBuffer(i, SizeOf(i));
  SetLength(Result, i);
  if i > 0 then
    AStream.ReadBuffer(Result[1], i);
end;

function ReadCheckString(AStream: TStream; const ACheckStr: String): Boolean;
var
  i: Integer;
  s: String;
begin
  Result := True;
  AStream.ReadBuffer(i, SizeOf(i));
  if i <> Length(ACheckStr) then
    Result := False
  else begin
    SetLength(s, i);
    AStream.ReadBuffer(s[1], i);
    if s <> ACheckStr then
      Result := False;
  end;
end;

{ TAstaIOIndex }

constructor TAstaIOIndex.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FRecs := TList.Create;
  FRecsOwned := True;
  FFieldList := TStringList.Create;
  FOptions := [ioNonMaintained];
end;

destructor TAstaIOIndex.Destroy;
begin
  Changing(irDeleted);
  Changed(irDeleted);
  if FRecsOwned then
    FRecs.Free;
  FRecs := nil;
  FFieldList.Free;
  FFieldList := nil;
  inherited Destroy;
end;

function TAstaIOIndex.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := FName;
end;

procedure TAstaIOIndex.Assign(AValue: TPersistent);
begin
  if AValue is TAstaIOIndex then begin
    Name := TAstaIOIndex(AValue).Name;
    Fields := TAstaIOIndex(AValue).Fields;
    Options := TAstaIOIndex(AValue).Options;
  end
  else
    inherited Assign(AValue);
end;

function TAstaIOIndex.GetCount: Integer;
begin
  if FRecs = nil then
    Result := 0
  else
    Result := FRecs.Count;
end;

function TAstaIOIndex.GetRecords(AIndex: Integer): TAstaDBListItem;
begin
  Result := TAstaDBListItem(FRecs[AIndex]);
end;

function TAstaIOIndex.GetDataSet: TDataSet;
begin
  Result := TAstaIOIndexes(Collection).DataSet;
end;

function TAstaIOIndex.GetByBookmark: Boolean;
begin
  Result := FFields = '';
end;

procedure TAstaIOIndex.SetByBookmark(AValue: Boolean);
begin
  if AValue then
    Fields := '';
end;

function TAstaIOIndex.GetUnique: Boolean;
begin
  Result := ([ioUnique, ioPrimary] * Options <> []);
end;

function TAstaIOIndex.GetActive: Boolean;
begin
  Result := not (ioNonMaintained in Options) and
    (TAstaIOIndexes(Collection).Active or ByBookmark and TAstaIOIndexes(Collection).FBMKActive) and
    (ByBookmark or (FieldCount > 0) and (FieldObj[0] <> nil)) and
    (not (ioNatural in Options) or Selected);
end;

procedure TAstaIOIndex.SetActive(const Value: Boolean);
begin
  if (not (ioNonMaintained in Options)) <> Value then
    if Value then
      Options := Options - [ioNonMaintained]
    else
      Options := Options + [ioNonMaintained];
end;

function TAstaIOIndex.GetActual: Boolean;
begin
  Result := (isActual in FStatuses);
end;

procedure TAstaIOIndex.FieldsDefined;
var
  i: Integer;
  fld: TField;
begin
  if (FieldCount = 0) or (FieldObj[0] <> nil) then
    Exit;
  if DataSet.FieldCount > 0 then begin
    Exclude(FStatuses, isHasCalcs);
    for i := 0 to FieldCount - 1 do begin
      fld := DataSet.FieldByName(FieldNames[i]);
      FFieldList.Objects[i] := fld;
      if fld.FieldKind <> fkData then
        Include(FStatuses, isHasCalcs);
    end;
  end;
  if Active then
    Rebuild
  else
    Exclude(FStatuses, isActual);
end;

procedure TAstaIOIndex.FieldsUnDefined;
var
  i: Integer;
begin
  for i := 0 to FieldCount - 1 do
    FFieldList.Objects[i] := nil;
end;

procedure TAstaIOIndex.UpdateFieldList;
var
  i: Integer;
begin
  FFieldList.Clear;
  i := 1;
  while i <= Length(FFields) do
    FFieldList.Add(ExtractFieldName(FFields, i));
end;

procedure TAstaIOIndex.SetFields(const Value: String);
begin
  if Fields <> Value then begin
    Changing(irDefinition);
    FFields := Value;
    UpdateFieldList;
    FieldsDefined;
    Changed(irDefinition);
  end;
end;

function TAstaIOIndex.GetFieldCount: Integer;
begin
  Result := FFieldList.Count;
end;

function TAstaIOIndex.GetFieldNames(AIndex: Integer): String;
begin
  Result := FFieldList[AIndex];
end;

function TAstaIOIndex.GetFieldObj(AIndex: Integer): TField;
begin
  Result := TField(FFieldList.Objects[AIndex]);
end;

procedure TAstaIOIndex.SetName(const Value: String);
begin
  Changing(irDefinition);
  FName := Value;
  Changed(irDefinition);
end;

{$HINTS OFF}
type
  __TCollection = class(TPersistent)
  private
    FItemClass: TCollectionItemClass;
    FItems: TList;
  end;
{$HINTS ON}

procedure TAstaIOIndex.SetOptions(const Value: TAstaIOIndexOptions);
begin
  if Options <> Value then begin
    Changing(irDefinition);
    FOptions := Value;
    if ioNatural in Options then
      DBListCreated
    else begin
      if not FRecsOwned then begin
        FRecs := TList.Create;
        FRecsOwned := True;
      end;
    end;
    if Active then
      try
        Rebuild;
      except
        Active := False;
        raise;
      end
    else
      Exclude(FStatuses, isActual);
    Changed(irDefinition);
  end;
end;

procedure TAstaIOIndex.SetDescendings(const Value: String);
begin
  if FDescendings <> Value then begin
    Changing(irDefinition);
    FDescendings := Value;
    Changed(irDefinition);
  end;
end;

procedure TAstaIOIndex.SetCaseInsensitives(const Value: String);
begin
  if FCaseInsensitives <> Value then begin
    Changing(irDefinition);
    FCaseInsensitives := Value;
    Changed(irDefinition);
  end;
end;

procedure TAstaIOIndex.LoadFromStream(AStream: TStream);
var
  i: Integer;
  j: Integer;
  astaDS: TAstaIOCustomDataSet;
  bmk: Integer;
begin
  Clear;
  FieldsUnDefined;
  ReadString(AStream);
  FName := ReadString(AStream);
  FFields := ReadString(AStream);
  ReadEnum(AStream, FOptions, SizeOf(FOptions));
  ReadEnum(AStream, FStatuses, SizeOf(FStatuses));
  if not (ioNatural in FOptions) then begin
    AStream.ReadBuffer(i, SizeOf(i));
    astaDS := TAstaIOCustomDataSet(DataSet);
    FRecs.Capacity := i;
    while i > 0 do begin
      AStream.ReadBuffer(bmk, SizeOf(bmk));
      if astaDS.FAstaList <> nil then begin
        j := astaDS.FAstaList.GetIndexFromBookMark(bmk);
        if j <> -1 then
          FRecs.Add(astaDS.FAstaList.Items[j])
        else
          Exclude(FStatuses, isActual);
      end;
      Dec(i);
    end;
  end;
  UpdateFieldList;
  FieldsDefined;
end;

procedure TAstaIOIndex.SaveToStream(AStream: TStream);
var
  i: Integer;
  pRec: TAstaDBListItem;
begin
  WriteString(AStream, ClassName);
  WriteString(AStream, FName);
  WriteString(AStream, FFields);
  WriteEnum(AStream, FOptions, SizeOf(FOptions));
  WriteEnum(AStream, FStatuses, SizeOf(FStatuses));
  if not (ioNatural in FOptions) then begin
    i := FRecs.Count;
    AStream.WriteBuffer(i, SizeOf(i));
    for i := 0 to FRecs.Count - 1 do begin
      pRec := TAstaDBListItem(FRecs[i]);
      AStream.WriteBuffer(pRec.FBookMark, SizeOf(pRec.FBookMark));
    end;
  end;
end;

function TAstaIOIndex.RecordDeleted(ARec: TAstaDBListItem): Integer;
begin
  if ioNatural in Options then
    Result := -1
  else if DataSet.Active then begin
    Result := FRecs.IndexOf(ARec);
    if Result <> -1 then begin
      FRecs.Delete(Result);
      if Result = FRecs.Count then
        Dec(Result);
    end
    else
      Exclude(FStatuses, isActual);
  end
  else begin
    Clear;
    Result := -1;
    Exclude(FStatuses, isActual);
  end;
end;

function TAstaIOIndex.RecordInserting(ARec: TAstaDBListItem): Integer;
begin
  FUpdatingRecordIndex := -1;
  Result := -1;
end;

function TAstaIOIndex.RecordInserted(ARec: TAstaDBListItem): Integer;
var
  i: Integer;
begin
  if Active and DataSet.Active then begin
    i := LocateRecord(ARec, -1, [loNearest, loEnd, loExclRec], [], Result);
    if i = 0 then
      if Unique then
        raise Exception.CreateFmt(SViolUniqueIndex, [Name, Fields]);
    if (ioDescending in Options) and not (ioNatural in Options) then
      Inc(Result);
    if ioNatural in Options then begin
      if Result < ARec.Index then
        Inc(Result);
      if Result >= FRecs.Count then
        Result := FRecs.Count - 1;
      ARec.Index := Result;
    end
    else if (Result < 0) or (Result >= FRecs.Count) then begin
      FRecs.Add(ARec);
      Result := FRecs.Count - 1;
    end
    else
      FRecs.Insert(Result, ARec);
  end
  else begin
    Result := -1;
    Exclude(FStatuses, isActual);
  end;
  Exclude(FStatuses, isUpdating);
end;

function TAstaIOIndex.RecordUpdating(ARec: TAstaDBListItem): Integer;
begin
  if Active and DataSet.Active then begin
    Result := FRecs.IndexOf(ARec);
    FUpdatingRecordIndex := Result;
    if Result = -1 then
      Exclude(FStatuses, isActual);
  end
  else begin
    Result := -1;
    Exclude(FStatuses, isActual);
  end;
end;

procedure TAstaIOIndex.RecordCheckUpdate(AField: TField);
begin
  if (AField.FieldKind = fkData) and (FFieldList.IndexOfObject(AField) <> -1) then
    Include(FStatuses, isUpdating);
end;

function TAstaIOIndex.RecordUpdated(ARec: TAstaDBListItem): Integer;
var
  delRec: Pointer;
begin
  if (isUpdating in FStatuses) or (isHasCalcs in FStatuses) then begin
    if Active and DataSet.Active then begin
      if ioNatural in Options then
        Result := RecordInserted(ARec)
      else begin
        delRec := nil;
        try
          if FUpdatingRecordIndex <> -1 then begin
            delRec := FRecs[FUpdatingRecordIndex];
            FRecs.Delete(FUpdatingRecordIndex);
          end;
          Result := RecordInserted(ARec);
        except
          if FUpdatingRecordIndex <> -1 then
            FRecs.Insert(FUpdatingRecordIndex, delRec);
          raise;
        end;
      end;
    end
    else begin
      Result := -1;
      Exclude(FStatuses, isActual);
    end;
    Exclude(FStatuses, isUpdating);
  end
  else
    Result := -1;
end;

function TAstaIOIndex.RecordCancel(ARec: TAstaDBListItem): Integer;
begin
  Result := FUpdatingRecordIndex;
  FUpdatingRecordIndex := -1;
  Exclude(FStatuses, isUpdating);
end;

procedure TAstaIOIndex.BeginUpdate;
begin
  Changing(irMaintenance);
  Inc(FUpdateCount);
end;

procedure TAstaIOIndex.EndUpdate;
begin
  if FUpdateCount > 0 then begin
    Dec(FUpdateCount);
    Changed(irMaintenance);
  end;
end;

procedure TAstaIOIndex.Changing(AReason: TAstaIOIndexReason);
begin
  if FUpdateCount = 0 then
    TAstaIOIndexes(Collection).IndexChanging(Self, AReason);
end;

procedure TAstaIOIndex.Changed(AReason: TAstaIOIndexReason);
begin
  if FUpdateCount = 0 then
    TAstaIOIndexes(Collection).IndexChanged(Self, AReason);
end;

procedure TAstaIOIndex.Rebuild;
begin
  if not (ioNatural in Options) or Selected then begin
    BeginUpdate;
    try
      Clear;
      if DataSet.Active then begin
        AddRecords;
        Sort;
        Include(FStatuses, isActual);
        TAstaIOCustomDataSet(DataSet).Aggregates.RecalcOnIndex(Name);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TAstaIOIndex.UpToDate;
begin
  if not Actual then
    Rebuild;
end;

procedure TAstaIOIndex.Clear;
begin
  if ioNatural in Options then
    Exit;
  Changing(irMaintenance);
  FRecs.Clear;
  Exclude(FStatuses, isActual);
  Changed(irMaintenance);
end;

procedure TAstaIOIndex.AddRecords;
var
  astaDS: TAstaIOCustomDataSet;
  i: Integer;
begin
  if ioNatural in Options then
    Exit;
  astaDS := TAstaIOCustomDataSet(DataSet);
  astaDS.FetchAll(False);
  if astaDS.FAstaList <> nil then begin
    Changing(irMaintenance);
    FRecs.Capacity := astaDS.FAstaList.Count;
    for i := 0 to astaDS.FAstaList.Count - 1 do
      FRecs.Add(astaDS.FAstaList[i]);
    Exclude(FStatuses, isActual);
    Changed(irMaintenance);
  end;
end;

function TAstaIOIndex.CompareRecs(ARec1, ARec2: TAstaDBListItem; AMatchingFields: Integer;
  AOptions: TAstaIOIndexCompareRecOptions): Integer;
var
  i, n: Integer;
  caseIns: Boolean;
begin
  Result := 0;
  if not ByBookmark then begin
    if AMatchingFields = -1 then
      n := FieldCount
    else
      n := AMatchingFields;
    for i := 0 to n - 1 do begin
      caseIns := crCaseInsensitive in AOptions;
      if (Length(FCaseInsensitives) >= i + 1) and (FCaseInsensitives[i + 1] <> ' ') then
        caseIns := True;
      Result := TAstaIOCustomDataSet(DataSet).CompareField(FieldObj[i], ARec1, ARec2,
        crPartial in AOptions, caseIns);
      if Result <> 0 then begin
        if (Length(FDescendings) >= i + 1) and (FDescendings[i + 1] <> ' ') then
          Result := - Result;
        Break;
      end;
    end;
  end;
  if (Result = 0) and (crUseBookmark in AOptions) or ByBookmark then
    if ARec1.FBookMark < ARec2.FBookMark then
      Result := -1
    else if ARec1.FBookMark > ARec2.FBookMark then
      Result := 1;
  if (crDescending in AOptions) and (FDescendings = '') then
    Result := - Result;
  if Result > 0 then
    Result := 1
  else if Result < 0 then
    Result := -1;
end;

procedure TAstaIOIndex.Sort;

  procedure DoQuickSort(left, right: Integer; AOpts: TAstaIOIndexCompareRecOptions);
  var
    i, j, p, newp: Longint;
    pivot: TAstaDBListItem;
  begin
    if left < right then begin
      FRecs.Exchange(left, (left + right) shr 1);
      i := left + 1;
      j := right;
      p := left;
      pivot := TAstaDBListItem(FRecs[p]);
      repeat
        while (i <= j) and (CompareRecs(TAstaDBListItem(FRecs[i]), pivot, -1, AOpts) < 0) do
          inc(i);
        while (i <= j) and (CompareRecs(TAstaDBListItem(FRecs[j]), pivot, -1, AOpts) > 0) do
          dec(j);
        if i < j then begin
          FRecs.Exchange(i, j);
          Inc(i);
          Dec(j);
        end;
      until i >= j;
      if (i = j) and (CompareRecs(TAstaDBListItem(FRecs[i]), pivot, -1, AOpts) > 0) then
        newp := i - 1
      else
        newp := j;
      if newp <> p then
        FRecs.Exchange(newp, p);
      if i > right then
        DoQuickSort(left, right - 1, AOpts)
      else begin
        if left < newp - 1 then
          DoQuickSort(left, newp - 1, AOpts);
        if newp + 1 < right then
          DoQuickSort(newp + 1, right, AOpts);
      end;
    end;
  end;

var
  opts: TAstaIOIndexCompareRecOptions;
  i: Integer;
begin
  if FRecs.Count >= 2 then begin
    Changing(irMaintenance);
    opts := [crUseBookmark];
    if ioDescending in Options then
      Include(opts, crDescending);
    if ioCaseInsensitive in Options then
      Include(opts, crCaseInsensitive);
    DoQuickSort(0, FRecs.Count - 1, opts);
    Changed(irMaintenance);
    if Unique then begin
      Exclude(opts, crUseBookmark);
      for i := 1 to FRecs.Count - 1 do
        if CompareRecs(TAstaDBListItem(FRecs[i - 1]), TAstaDBListItem(FRecs[i]), -1, opts) = 0 then
          raise Exception.CreateFmt(SDuplRecInUniqueIndex, [Name, Fields]);
    end;
  end;
end;

// ??? this procedure must be optimized ...
//     vvvv

function TAstaIOIndex.LocateRecord(ARec: TAstaDBListItem; AMatchingFields: Integer;
  AAstaOptions: TAstaIOLocateOptions; AOptions: TLocateOptions;
  var APosition: Integer): Integer;

  function LookAfterSearch(ARec: TAstaDBListItem; ABackward, ATheSame: Boolean;
    AOpts: TAstaIOIndexCompareRecOptions; var APosition: Integer): Boolean;
  var
    astaDS: __TAstaIOCustomDataSet;
    i, goodI: Integer;
  begin
    astaDS := __TAstaIOCustomDataSet(DataSet);
    i := APosition;
    if ABackward then begin
      goodI := -1;
      repeat
        if not (crFiltered in AOpts) or astaDS.DirectFilter(TAstaDBListItem(FRecs[i])) then
          goodI := i;
        Dec(i);
      until (i < 0) or
            not ATheSame and
              (goodI <> -1) or
            ATheSame and
              (CompareRecs(ARec, TAstaDBListItem(FRecs[i]), AMatchingFields, AOpts) <> 0);
      Result := (goodI <> - 1);
    end
    else begin
      goodI := -1;
      repeat
        if not (crFiltered in AOpts) or astaDS.DirectFilter(TAstaDBListItem(FRecs[i])) then
          goodI := i;
        Inc(i);
      until (i >= FRecs.Count) or
            ATheSame and
              (CompareRecs(ARec, TAstaDBListItem(FRecs[i]), AMatchingFields, AOpts) <> 0) or
            not ATheSame and
              (goodI <> -1);
      Result := (goodI <> - 1);
    end;
    APosition := goodI;
  end;

  function DoBinarySearch(ARec: TAstaDBListItem; AOpts: TAstaIOIndexCompareRecOptions;
    left, right: Integer; var APosition: Integer): Integer;
  var
    mid: Integer;
    pRec2: TAstaDBListItem;
    noRec: Boolean;
  begin
    Result := -1;
    repeat
      mid := (right + left + 1) div 2;
      pRec2 := TAstaDBListItem(FRecs[mid]);
      noRec := False;
      if (crExclRec in AOpts) and (ARec = pRec2) then begin
        if mid < right then
          Inc(mid)
        else if mid > left then
          Dec(mid)
        else
          noRec := True;
        if not noRec then
          pRec2 := TAstaDBListItem(FRecs[mid]);
      end;
      if not noRec then
        Result := CompareRecs(ARec, pRec2, AMatchingFields, AOpts);
      if Result = 0 then
        APosition := mid
      else begin
        if Result < 0 then
          right := mid - 1
        else
          left := mid + 1;
        if left > right then begin
          if (APosition < 0) and (crNearest in AOpts) then begin
            while (Result < 0) and (crDescending in AOpts) or
                  (Result > 0) and not (crDescending in AOpts) do begin
              if crDescending in AOpts then
                Dec(mid)
              else
                Inc(mid);
              if (mid < 0) or (mid >= FRecs.Count) then
                Break;
              pRec2 := TAstaDBListItem(FRecs[mid]);
              Result := CompareRecs(ARec, pRec2, AMatchingFields, AOpts);
            end;
            APosition := Mid;
            Result := 1;
          end;
        end;
      end;
    until (Result = 0) or (left > right);
  end;

var
  opts: TAstaIOIndexCompareRecOptions;
  i: Integer;
begin
  APosition := -1;
  if FRecs.Count > 0 then begin
    opts := [];
    if ioDescending in Options then
      Include(opts, crDescending);
    if (loCaseInsensitive in AOptions) or (ioCaseInsensitive in Options) then
      Include(opts, crCaseInsensitive);
    if loPartialKey in AOptions then
      Include(opts, crPartial);
    if loNearest in AAstaOptions then
      Include(opts, crNearest);
    if loFiltered in AAstaOptions then
      Include(opts, crFiltered);
    if loExclRec in AAstaOptions then
      Include(opts, crExclRec);
    // ??? here we should check - if index is Actual then BinarySearch
    // else SequentialSearch
    Result := DoBinarySearch(ARec, opts, 0, FRecs.Count - 1, APosition);
    if (APosition >= 0) and (APosition <= FRecs.Count - 1) then begin
      if Result <> 0 then begin
        i := APosition;
        if LookAfterSearch(ARec, crDescending in opts, False, opts, i) then
          APosition := i
        else if crDescending in opts then
          APosition := -1
        else
          APosition := FRecs.Count;
      end
      else begin
        i := APosition;
        if LookAfterSearch(ARec, {not (crDescending in opts) and} not (loEnd in AAstaOptions), True, opts, i) then
          APosition := i
        else if not (Unique and ((AMatchingFields = -1) or (AMatchingFields = FieldCount))) then begin
          i := APosition;
          if LookAfterSearch(ARec, {(crDescending in opts) or} (loEnd in AAstaOptions), True, opts, i) then
            APosition := i
          else begin
            if i < APosition then
              Result := -1
            else
              Result := 1;
            if crDescending in opts then
              Result := - Result;
            APosition := -1;
          end;
        end;
      end;
    end;
  end
  else
    Result := 1;
end;

function TAstaIOIndex.LocateByBookmark(ABmk: Integer; AAstaOptions: TAstaIOLocateOptions;
  AOptions: TLocateOptions; var APosition: Integer): Integer;
var
  i: Integer;
  tmpItem: TAstaDBListItem;
  astaDS: __TAstaIOCustomDataSet;
begin
  astaDS := __TAstaIOCustomDataSet(DataSet);
  if ByBookmark then begin
    tmpItem := TAstaDBListItem.CreateForLocate(astaDS.FAstaList, 0);
    try
      tmpItem.FBookMark := ABmk;
      Result := LocateRecord(tmpItem, -1, AAstaOptions, AOptions, APosition);
    finally
      tmpItem.Free;
    end;
  end
  else begin
    APosition := -1;
    Result := 1;
    for i := 0 to FRecs.Count - 1 do
      if (TAstaDBListItem(FRecs[i]).FBookMark = ABmk) and
         (not (loFiltered in AAstaOptions) or astaDS.DirectFilter(TAstaDBListItem(FRecs[i]))) then begin
        Result := 0;
        APosition := i;
        Break;
      end;
  end;
end;

function TAstaIOIndex.GetSelected: Boolean;
begin
  Result := (TAstaIOIndexes(Collection).SelectedIndex = Self);
end;

procedure TAstaIOIndex.SetSelected(const Value: Boolean);
begin
  if Value <> Selected then
    if Value then
      TAstaIOIndexes(Collection).SetSelectedIndexNo(Index, False)
    else
      TAstaIOIndexes(Collection).SetSelectedIndexNo(-1, False);
end;

procedure TAstaIOIndex.DBListCreated;
begin
  if ioNatural in Options then begin
    if FRecsOwned and (FRecs <> nil) then begin
      FRecs.Free;
      FRecs := nil;
    end;
    if (DataSet <> nil) and (TAstaIOCustomDataSet(DataSet).FAstaList <> nil) then
      FRecs := __TCollection(TAstaIOCustomDataSet(DataSet).FAstaList).FItems;
    FRecsOwned := False;
  end;
end;

procedure TAstaIOIndex.DBListDestroyed;
begin
  if ioNatural in Options then
    FRecs := nil;
  Clear;
end;

{ TAstaIOIndexes }

constructor TAstaIOIndexes.Create(AOwner: TPersistent);
begin
  inherited Create(TAstaIOIndex);
  if AOwner <> nil then
    FDataSet := AOwner as TAstaIOCustomDataSet;
  FPosition := 0;
  FSelectedIndex := -1;
  FActive := True;
end;

function TAstaIOIndexes.GetOwner: TPersistent;
begin
  Result := FDataSet;
end;

function TAstaIOIndexes.GetIndex(AIndex: Integer): TAstaIOIndex;
begin
  Result := TAstaIOIndex(inherited Items[AIndex]);
end;

function TAstaIOIndexes.Add: TAstaIOIndex;
begin
  Result := TAstaIOIndex(inherited Add);
end;

procedure TAstaIOIndexes.IndexChanging(AIndex: TAstaIOIndex; AReason: TAstaIOIndexReason);
begin
  if (AIndex = nil) or AIndex.Selected then begin
    if AReason <> irDeleted then
      FBmk := Bookmark;
    if AReason <> irMaintenance then
      if DataSet.Active then
        __TAstaIOCustomDataSet(DataSet).CancelRange;
  end;
  if Assigned(FOnChanging) then
    FOnChanging(AIndex, AReason);
end;

procedure TAstaIOIndexes.IndexChanged(AIndex: TAstaIOIndex; AReason: TAstaIOIndexReason);
begin
  if (AIndex = nil) or AIndex.Selected then begin
    if (AReason <> irDeleted) and not (csDestroying in DataSet.ComponentState) then begin
      Bookmark := FBmk;
      if DataSet.Active and (__TAstaIOCustomDataSet(DataSet).BufferCount > 0) and
         (__TAstaIOCustomDataSet(DataSet).RecordCount > 0) then begin
        DataSet.UpdateCursorPos;
        DataSet.Resync([]);
      end;
    end
    else
      SetSelectedIndexNo(-1, False);
  end
  else if (AIndex <> nil) and (AReason = irDeleted) then begin
    if AIndex.Index < FSelectedIndex then
      Dec(FSelectedIndex)
    else if AIndex.Index = FSelectedIndex then
      SetSelectedIndexNo(-1, False);
  end;
  if Assigned(FOnChanged) then
    FOnChanged(AIndex, AReason);
end;

function TAstaIOIndexes.GetOrdered: Boolean;
begin
  Result := (FSelectedIndex <> -1);
end;

procedure TAstaIOIndexes.SetActive(const Value: Boolean);
begin
  if FActive <> Value then begin
    FActive := Value;
    if FActive then
      UpToDate;
  end;
end;

function TAstaIOIndexes.FindIndex(const AName: String): TAstaIOIndex;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].Name, AName) = 0 then begin
      Result := Items[i];
      Break;
    end;
end;

function TAstaIOIndexes.IndexByName(const AName: String): TAstaIOIndex;
begin
  Result := FindIndex(AName);
  if Result = nil then
    raise Exception.CreateFmt(SIndexNotFound, [AName]);
end;

function TAstaIOIndexes.IndexSure(const AName: String): TAstaIOIndex;
begin
  Result := FindIndex(AName);
  if Result = nil then begin
    Result := Add;
    Result.Name := AName;
  end;
end;

function TAstaIOIndexes.IndexForBookmark: TAstaIOIndex;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Actual and Items[i].ByBookmark then begin
      Result := Items[i];
      Break;
    end;
end;

function TAstaIOIndexes.IndexForFields(const AFields: String;
  AOptions: TAstaIOIndexOptions; var AMatchingFields: Integer;
  AActive: Integer): TAstaIOIndex;
var
  s: String;
  i, j: Integer;
  firstUnique: TAstaIOIndex;
begin
  Result := nil;
  firstUnique := nil;
  s := AnsiUpperCase(AFields);
  for i := 0 to Count - 1 do
    with Items[i] do
      if (([ioPrimary, ioUnique] * AOptions <> []) and Unique and
          (Options * AOptions - [ioNonMaintained] = AOptions - [ioNonMaintained])) and
         ((AActive = 0) or (AActive < 0) and not Active or (AActive > 0) and Active) and
         (not (ioNatural in Options) or Selected) and
         ((s = '') or
          (Pos(s, AnsiUpperCase(Fields)) = 1) and
          ((Length(s) = Length(Fields)) or (Fields[Length(s) + 1] = ';'))) then begin
        if Length(AFields) = Length(Fields) then
          AMatchingFields := -1
        else begin
          AMatchingFields := 1;
          for j := 1 to Length(AFields) do
            if AFields[j] = ';' then
              Inc(AMatchingFields);
        end;
        if [ioPrimary, ioUnique] * AOptions = [ioPrimary, ioUnique] then begin
          if ioPrimary in Options then begin
            Result := Items[i];
            Break;
          end;
          if firstUnique = nil then
            firstUnique := Items[i];
        end
        else begin
          Result := Items[i];
          Break;
        end;
      end;
  if firstUnique <> nil then
    Result := firstUnique;
end;

function TAstaIOIndexes.LocateByBookmark(ABmk: Integer; AAstaOptions: TAstaIOLocateOptions;
  AOptions: TLocateOptions; var APosition: Integer): Integer;
var
  astaDS: __TAstaIOCustomDataSet;
begin
  if Ordered then
    Result := SelectedIndex.LocateByBookmark(ABmk, AAstaOptions, AOptions, APosition)
  else begin
    astaDS := __TAstaIOCustomDataSet(DataSet);
    Result := 1;
    APosition := -1;
    if astaDS.FAstaList <> nil then begin
      APosition := astaDS.FAstaList.GetIndexFromBookMark(ABmk);
      if APosition <> -1 then
        if (loFiltered in AAstaOptions) and
           not astaDS.DirectFilter(TAstaDBListItem(astaDS.FAstaList.Items[APosition])) then begin
          Result := 1;
          APosition := -1;
        end
        else
          Result := 0;
    end;
  end;
end;

function TAstaIOIndexes.LocateRecord(ARec: TAstaDBListItem; const AFields: String;
  AAstaOptions: TAstaIOLocateOptions; AOptions: TLocateOptions; var APosition: Integer): Integer;
var
  ind: TAstaIOIndex;
  astaDS: __TAstaIOCustomDataSet;
  j: Integer;
  lst: TList;
  indOpts: TAstaIOIndexOptions;
  prevPos: Integer;
  fldCnt: Integer;
begin
  APosition := -1;
  Result := 1;
  indOpts := [];
  astaDS := __TAstaIOCustomDataSet(DataSet);
  if loCaseInsensitive in AOptions then
    Include(indOpts, ioCaseInsensitive);
  ind := IndexForFields(AFields, indOpts, fldCnt, 1);
  if (ind <> nil) and
     (ind.Selected or
      (not (loPartialKey in AOptions) and
       ((fldCnt = -1) or (fldCnt = ind.FieldCount)) and
       ind.Unique
      )) then begin
    Result := ind.LocateRecord(ARec, fldCnt, AAstaOptions, AOptions, APosition);
    if not ind.Selected then
      if (Result = 0) and (APosition <> -1) then
        LocateByBookmark(TAstaDBListItem(ind.FRecs[APosition]).FBookMark, [], [], APosition)
      else begin
        Result := 1;
        APosition := -1;
      end;
  end
  else if astaDS.FAstaList <> nil then begin
    lst := TList.Create;
    prevPos := Position;
    try
      astaDS.GetFieldList(lst, AFields);
      Position := 0;
      while IsPositionValid do begin
        for j := 0 to lst.Count - 1 do begin
          if Rec.FBookMark >= 0 then begin
            Result := astaDS.CompareField(TField(lst[j]), ARec, Rec,
              loPartialKey in AOptions, loCaseInsensitive in AOptions);
            if Result <> 0 then
              Break;
          end;
        end;
        if Result <> 0 then
          Position := Position + 1
        else if (loFiltered in AAstaOptions) and not astaDS.DirectFilter(Rec) then begin
          Result := 1;
          Position := Position + 1;
        end
        else begin
          APosition := Position;
          Break;
        end;
      end;
    finally
      Position := prevPos;
      lst.Free;
    end;
  end;
end;

function TAstaIOIndexes.GetSelectedIndexFields: String;
begin
  if FSelectedIndex <> -1 then
    Result := Items[FSelectedIndex].Fields
  else
    Result := '';
end;

function TAstaIOIndexes.GetSelectedIndexName: String;
begin
  if FSelectedIndex <> -1 then
    Result := Items[FSelectedIndex].Name
  else
    Result := '';
end;

procedure TAstaIOIndexes.SetSelectedIndexFields(const Value: String);
var
  ind: TAstaIOIndex;
  n: Integer;
begin
  if SelectedIndexFields <> Value then begin
    if Value = '' then
      SetSelectedIndexNo(-1, False)
    else begin
      ind := IndexForFields(Value, [], n, 0);
      if ind <> nil then
        SetSelectedIndexNo(ind.Index, False)
      else begin
        ind := IndexSure(SASTADefIndexName);
        ind.Fields := Value;
        ind.Options := [];
        SetSelectedIndexNo(ind.Index, True);
      end;
    end;
  end;
end;

procedure TAstaIOIndexes.SetSelectedIndexName(const Value: String);
begin
  if SelectedIndexName <> Value then begin
    if Value = '' then
      SetSelectedIndexNo(-1, False)
    else
      SetSelectedIndexNo(IndexByName(Value).Index, False);
  end;
end;

procedure TAstaIOIndexes.SetSelectedIndexNo(const Value: Integer; AForce: Boolean);
begin
  if (Value <> FSelectedIndex) or AForce then begin
    IndexChanging(SelectedIndex, irSelected);
    FSelectedIndex := Value;
    if (FSelectedIndex <> -1) and (ioNatural in SelectedIndex.Options) and
       not (csDestroying in DataSet.ComponentState) then
      SelectedIndex.Rebuild;
    IndexChanged(SelectedIndex, irSelected);
  end;
end;

function TAstaIOIndexes.GetSelectedIndex: TAstaIOIndex;
begin
  if FSelectedIndex <> -1 then
    Result := Items[FSelectedIndex]
  else
    Result := nil;
end;

function TAstaIOIndexes.GetIsPositionValid: Boolean;
var
  astaDS: TAstaIOCustomDataSet;
begin
  Result := (Position >= 0) and (Position < RecsCount);
  if Result then begin
    astaDS := TAstaIOCustomDataSet(DataSet);
    if astaDS.FAstaList = nil then
      Result := False
    else
      Result := astaDS.FAstaList.Count > FPosition;
  end;
end;




function TAstaIOIndexes.GetRec: TAstaDBListItem;
var
  astaDS: TAstaIOCustomDataSet;
begin
  if not IsPositionValid then
    raise Exception.CreateFmt(SNoRecAtPos, [Position]);
  if Ordered then
    Result := SelectedIndex.Recs[Position]
  else begin
    astaDS := TAstaIOCustomDataSet(DataSet);
    if astaDS.FAstaList = nil then
      raise Exception.CreateFmt(SNoRecAtPos, [Position])
    else
      Result := astaDS.FAstaList.Items[Position];
  end;
end;

function TAstaIOIndexes.GetRecsCount: Integer;
var
  astaDS: TAstaIOCustomDataSet;
begin
  if Ordered then
    Result := SelectedIndex.RecsCount
  else begin
    astaDS := TAstaIOCustomDataSet(DataSet);
    if astaDS.FAstaList = nil then
      Result := 0
    else
      Result := astaDS.FAstaList.Count;
  end;
end;

function TAstaIOIndexes.GetBookmark: Integer;
begin
  if not IsPositionValid then
    Result := -1
  else
    Result := Rec.FBookMark
end;

procedure TAstaIOIndexes.SetBookmark(const Value: Integer);
var
  i: Integer;
begin
  if Bookmark <> Value then
    if LocateByBookmark(Value, [], [], i) = 0 then
      SetPosition(i);
end;

procedure TAstaIOIndexes.SetPosition(const Value: Integer);
begin
  if (Position <> Value) or not IsPositionValid then begin
    if Value < 0 then
      FPosition := -1
    else if Value >= RecsCount then
      FPosition := RecsCount
    else
      FPosition := Value;
  end;
end;

procedure TAstaIOIndexes.RecordCancel(ARec: TAstaDBListItem);
var
  i, p: Integer;
begin
  for i := 0 to Count - 1 do begin
    p := Items[i].RecordCancel(ARec);
    if (p <> -1) and (Items[i] = SelectedIndex) then
      Position := p;
  end;
end;

procedure TAstaIOIndexes.RecordDeleted(ARec: TAstaDBListItem);
var
  i, p: Integer;
begin
  for i := 0 to Count - 1 do begin
    p := Items[i].RecordDeleted(ARec);
    if (p <> -1) and (Items[i] = SelectedIndex) then
      Position := p;
  end;
end;

procedure TAstaIOIndexes.RecordInserting(ARec: TAstaDBListItem);
var
  i, p: Integer;
begin
  for i := 0 to Count - 1 do begin
    p := Items[i].RecordInserting(ARec);
    if (p <> -1) and (Items[i] = SelectedIndex) then
      Position := p;
  end;
end;

procedure TAstaIOIndexes.RecordInserted(ARec: TAstaDBListItem);
var
  i, p: Integer;
begin
  for i := 0 to Count - 1 do begin
    p := Items[i].RecordInserted(ARec);
    if (p <> -1) and (Items[i] = SelectedIndex) then
      Position := p;
  end;
end;

procedure TAstaIOIndexes.RecordUpdating(ARec: TAstaDBListItem);
var
  i, p: Integer;
begin
  for i := 0 to Count - 1 do begin
    p := Items[i].RecordUpdating(ARec);
    if (p <> -1) and (Items[i] = SelectedIndex) then
      Position := p;
  end;
end;

procedure TAstaIOIndexes.RecordCheckUpdate(AField: TField);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].RecordCheckUpdate(AField);
end;

procedure TAstaIOIndexes.RecordUpdated(ARec: TAstaDBListItem);
var
  i, p: Integer;
begin
  for i := 0 to Count - 1 do begin
    p := Items[i].RecordUpdated(ARec);
    if (p <> -1) and (Items[i] = SelectedIndex) then
      Position := p;
  end;
end;

procedure TAstaIOIndexes.Rebuild;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Rebuild;
end;

procedure TAstaIOIndexes.UpToDate;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Active then
      Items[i].UpToDate;
end;

procedure TAstaIOIndexes.LoadFromStream(AStream: TStream);
var
  i: Integer;
  strPos: LongInt;
  selInd: String;
begin
  Clear;
  strPos := AStream.Position;
  try
    if not ReadCheckString(AStream, ClassName) then
      Abort;
  except
    AStream.Position := strPos;
    Exit;
  end;
  AStream.ReadBuffer(FActive, SizeOf(FActive));
  selInd := ReadString(AStream);
  AStream.ReadBuffer(i, SizeOf(i));
  while i > 0 do begin
    Add.LoadFromStream(AStream);
    Dec(i);
  end;
  SelectedIndexName := selInd;
end;

procedure TAstaIOIndexes.SaveToStream(AStream: TStream);
var
  i: Integer;
begin
  WriteString(AStream, ClassName);
  AStream.WriteBuffer(FActive, SizeOf(FActive));
  WriteString(AStream, SelectedIndexName);
  i := Count;
  AStream.WriteBuffer(i, SizeOf(i));
  for i := 0 to Count - 1 do
    Items[i].SaveToStream(AStream);
end;

procedure TAstaIOIndexes.FieldsDefined;
var
  astaDS: TAstaIOCustomDataSet;
  i: Integer;
  fetched: Boolean;
begin
  astaDS := TAstaIOCustomDataSet(DataSet);
  fetched := False;
  for i := 0 to Count - 1 do begin
    if not fetched and Items[i].Active then begin
      astaDS.FetchAll(False);
      fetched := True;
    end;
    Items[i].FieldsDefined;
  end;
end;

procedure TAstaIOIndexes.FieldsUnDefined;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].FieldsUnDefined;
end;

procedure TAstaIOIndexes.DBListCreated;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].DBListCreated;
end;

procedure TAstaIOIndexes.DBListDestroyed;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].DBListDestroyed;
end;

{ TAstaIOIndexDefs }

constructor TAstaIOIndexDefs.Create(ADataSet: TDataSet;
  AIndexes: TAstaIOIndexes);
begin
  inherited Create(ADataSet);
  FIndexes := AIndexes;
  OnUpdate := DoUpdate;
end;

procedure TAstaIOIndexDefs.DoUpdate(ASender: TObject);
begin
  Defs2Indexes;
end;

procedure TAstaIOIndexDefs.Defs2Indexes;
var
  i: Integer;
  opt: TAstaIOIndexOptions;
begin
  if FSelfUpdating then
    Exit;
  FSelfUpdating := True;
  try
    for i := 0 to Count - 1 do
      with FIndexes.IndexSure(Items[i].Name) do begin
        Name := Items[i].Name;
        Fields := Items[i].Fields;
        Descendings := Items[i].DescFields;
        CaseInsensitives := Items[i].CaseInsFields;
        opt := [];
        if ixPrimary in Items[i].Options then
          Include(opt, ioPrimary);
        if ixUnique in Items[i].Options then
          Include(opt, ioUnique);
        if ixDescending in Items[i].Options then
          Include(opt, ioDescending);
        if ixCaseInsensitive in Items[i].Options then
          Include(opt, ioCaseInsensitive);
        if ixNonMaintained in Items[i].Options then
          Include(opt, ioNonMaintained);
        Options := opt;
      end;
    i := 0;
    while i < FIndexes.Count do
      if Find(FIndexes[i].Name) = nil then
        FIndexes.Delete(i)
      else
        Inc(i);
  finally
    FSelfUpdating := False;
  end;
end;

procedure TAstaIOIndexDefs.Indexes2Defs;
var
  i: Integer;
  opt: TIndexOptions;
begin
  if FSelfUpdating then
    Exit;
  FSelfUpdating := True;
  try
    Clear;
    for i := 0 to FIndexes.Count - 1 do
      with AddIndexDef do begin
        Name := FIndexes[i].Name;
        Fields := FIndexes[i].Fields;
        DescFields := '';
        opt := [];
        if ioPrimary in FIndexes[i].Options then
          Include(opt, ixPrimary);
        if ioUnique in FIndexes[i].Options then
          Include(opt, ixUnique);
        if ioDescending in FIndexes[i].Options then
          Include(opt, ixDescending);
        if ioCaseInsensitive in FIndexes[i].Options then
          Include(opt, ixCaseInsensitive);
        if ioNonMaintained in FIndexes[i].Options then
          Include(opt, ixNonMaintained);
        Options := opt;
      end;
  finally
    Updated := True;
    FSelfUpdating := False;
  end;
end;

{ TAstaIOAggregateValue }

procedure TAstaIOAggregateValue.Attach;
begin
  Inc(FUsageCount);
end;

procedure TAstaIOAggregateValue.Detach;
begin
  Dec(FUsageCount);
  if FUsageCount <= 0 then
    Free;
end;

{ TAstaIOAggregate }

constructor TAstaIOAggregate.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAggKind := akCount;
  FValueIndex := -1;
  FCountIndex := -1;
end;

procedure TAstaIOAggregate.Assign(AValue: TPersistent);
begin
  if AValue is TAstaIOAggregate then begin
    FName := TAstaIOAggregate(AValue).FName;
    FieldName := TAstaIOAggregate(AValue).FieldName;
    AggKind := TAstaIOAggregate(AValue).AggKind;
    IndexName := TAstaIOAggregate(AValue).IndexName;
    Level := TAstaIOAggregate(AValue).Level;
    Active := TAstaIOAggregate(AValue).Active;
  end
  else
    inherited Assign(AValue);
end;

function TAstaIOAggregate.BuildName: String;
begin
  case AggKind of
  akSum: Result := 'SUM';
  akAvg: Result := 'AVG';
  akCount: Result := 'COUNT';
  akMin: Result := 'MIN';
  akMax: Result := 'MAX';
  end;
  Result := Result + '(';
  if FieldName <> '' then
    Result := Result + FieldName
  else
    Result := Result + '*';
  Result := Result + ')';
end;

function TAstaIOAggregate.GetDisplayName: String;
begin
  Result := Name;
end;

function TAstaIOAggregate.GetName: String;
begin
  if FName = '' then
    Result := BuildName
  else
    Result := FName;
end;

procedure TAstaIOAggregate.SetName(const Value: String);
begin
  if BuildName = Value then
    FName := ''
  else
    FName := Value;
end;

function TAstaIOAggregate.GetActual: Boolean;
begin
  Result := (asActual in FState);
end;

function TAstaIOAggregate.GetRunning: Boolean;
begin
  Result := Active and TAstaIOAggregates(Collection).Active and
    DataSet.Active and
    ((FieldName <> '') and (FAggField <> nil) or (AggKind = akCount)) and
    ((IndexName = '') or
     TAstaIOAggregates(Collection).FIndexes.IndexByName(IndexName).Active);
end;

procedure TAstaIOAggregate.SetActive(const Value: Boolean);
begin
  if Active <> Value then begin
    FActive := Value;
    Recalc;
  end;
end;

procedure TAstaIOAggregate.SetAggKind(const Value: TAstaIOAggKind);
begin
  if FAggKind <> Value then begin
    FAggKind := Value;
    Recalc;
  end;
end;

procedure TAstaIOAggregate.SetFieldName(const Value: String);
begin
  if FieldName <> Value then begin
    FFieldName := Value;
    FieldsDefined;
  end;
end;

procedure TAstaIOAggregate.SetIndexName(const Value: String);
begin
  if IndexName <> Value then begin
    FIndexName := Value;
    if IndexName = '' then
      FLevel := 0;
    Recalc;
  end;
end;

procedure TAstaIOAggregate.SetLevel(const Value: Integer);
begin
  if FLevel <> Value then begin
    FLevel := Value;
    if IndexName = '' then
      FLevel := 0;
    Recalc;
  end;
end;

procedure TAstaIOAggregate.FieldsDefined;
begin
  if (FieldName = '') and (AggKind = akCount) or (DataSet.FieldCount = 0) then
    FAggField := nil
  else
    FAggField := DataSet.FieldByName(FieldName);
  Recalc;
end;

procedure TAstaIOAggregate.FieldsUnDefined;
begin
  FAggField := nil;
end;

function TAstaIOAggregate.GetDataSet: TDataSet;
begin
  Result := TAstaIOCustomDataSet(TAstaIOAggregates(Collection).FIndexes.DataSet);
end;

procedure TAstaIOAggregate.Clear;
begin
  if FValueIndex <> -1 then
    TAstaIOAggregates(Collection).DeleteValues(FValueIndex);
  if FCountIndex <> -1 then
    TAstaIOAggregates(Collection).DeleteValues(FCountIndex);
  Exclude(FState, asActual);
end;

procedure TAstaIOAggregate.AllocAggVals(var AValObj, ACntObj: TAstaIOAggregateValue);
begin
  AValObj := TAstaIOAggregateValue.Create;
  if AggKind = akCount then
    AValObj.Value := 0
  else
    AValObj.Value := Null;
  ACntObj := nil;
  if AggKind = akAvg then begin
    ACntObj := TAstaIOAggregateValue.Create;
    ACntObj.Value := 0;
  end;
end;

procedure TAstaIOAggregate.GetAggVals(ARec: TastaDBListitem; var AValObj, ACntObj: TAstaIOAggregateValue);
begin
  AValObj := ARec.FAggs^[FValueIndex];
  if AggKind = akAvg then
    ACntObj := ARec.FAggs^[FCountIndex]
  else
    ACntObj := nil;
end;

procedure TAstaIOAggregate.AttachAggVals(ARec: TastaDBListitem; AValObj, ACntObj: TAstaIOAggregateValue);
begin
  AValObj.Attach;
  ARec.FAggs^[FValueIndex] := AValObj;
  if AggKind = akAvg then begin
    ACntObj.Attach;
    ARec.FAggs^[FCountIndex] := ACntObj;
  end;
end;

procedure TAstaIOAggregate.DetachAggVals(ARec: TastaDBListitem);
begin
  if ARec.FAggs^[FValueIndex] <> nil then begin
    TAstaIOAggregateValue(ARec.FAggs^[FValueIndex]).Detach;
    ARec.FAggs^[FValueIndex] := nil;
  end;
  if AggKind = akAvg then
    if ARec.FAggs^[FCountIndex] <> nil then begin
      TAstaIOAggregateValue(ARec.FAggs^[FCountIndex]).Detach;
      ARec.FAggs^[FCountIndex] := nil;
    end;
end;

function FixFMTBcdAdd(const V1, V2: Variant): Variant;
{$ifdef Delphi6AndUp}
var
  C1, C2: Currency;
  B: TBcd;
{$endif}
begin
{$ifdef Delphi6AndUp}
  if ((VarType(V1) = VarFMTBcd) or (VarType(V2) = VarFMTBcd)) and
     BCDToCurr(VarToBcd(V1), C1) and BCDToCurr(VarToBcd(V2), C2) and CurrToBCD(C1 + C2, B) then
    Result := VarFMTBcdCreate(B)
  else
{$endif}
    Result := V1 + V2;
end;

function FixFMTBcdSub(const V1, V2: Variant): Variant;
{$ifdef Delphi6AndUp}
var
  C1, C2: Currency;
  B: TBcd;
{$endif}
begin
{$ifdef Delphi6AndUp}
  if ((VarType(V1) = VarFMTBcd) or (VarType(V2) = VarFMTBcd)) and
     BCDToCurr(VarToBcd(V1), C1) and BCDToCurr(VarToBcd(V2), C2) and CurrToBCD(C1 - C2, B) then
    Result := VarFMTBcdCreate(B)
  else
{$endif}
    Result := V1 - V2;
end;

procedure TAstaIOAggregate.IncAggVals(AVal: Variant; AValObj, ACntObj: TAstaIOAggregateValue);
begin
  case AggKind of
  akSum:
    if VarIsNull(AValObj.Value) then
      AValObj.Value := AVal
    else if not VarIsNull(AVal) then
      AValObj.Value := FixFMTBcdAdd(AValObj.Value, AVal);
  akAvg:
    begin
      if VarIsNull(AValObj.Value) then
        AValObj.Value := AVal
      else if not VarIsNull(AVal) then
        AValObj.Value := FixFMTBcdAdd(AValObj.Value, AVal);
      if not VarIsNull(AVal) then
        ACntObj.Value := ACntObj.Value + 1;
    end;
  akCount:
    if not VarIsNull(AVal) then
      AValObj.Value := AValObj.Value + 1;
  akMin:
    if VarIsNull(AValObj.Value) then
      AValObj.Value := AVal
    else if not VarIsNull(AVal) and (AValObj.Value > AVal) then
      AValObj.Value := AVal;
  akMax:
    if VarIsNull(AValObj.Value) then
      AValObj.Value := AVal
    else if not VarIsNull(AVal) and (AValObj.Value < AVal) then
      AValObj.Value := AVal
  end;
end;

procedure TAstaIOAggregate.DecAggVals(AVal: Variant; AValObj, ACntObj: TAstaIOAggregateValue);
begin
  case AggKind of
  akSum:
    if not VarIsNull(AValObj.Value) and not VarIsNull(AVal) then
      AValObj.Value := FixFMTBcdSub(AValObj.Value, AVal);
  akAvg:
    begin
      if not VarIsNull(AValObj.Value) and not VarIsNull(AVal) then
        AValObj.Value := FixFMTBcdSub(AValObj.Value, AVal);
      if not VarIsNull(ACntObj.Value) and not VarIsNull(AVal) then
        ACntObj.Value := ACntObj.Value - 1;
    end;
  akCount:
    if not VarIsNull(AValObj.Value) and not VarIsNull(AVal) then
      AValObj.Value := AValObj.Value - 1;
  akMin: ; // recalc only
  akMax: ; // recalc only
  end;
end;

procedure TAstaIOAggregate.Calculate;
var
  i: Integer;
  valObj: TAstaIOAggregateValue;
  cntObj: TAstaIOAggregateValue;
  ind: TAstaIOIndex;
  astaDS: __TAstaIOCustomDataSet;
  aggs: TAstaIOAggregates;
  opts: TAstaIOIndexCompareRecOptions;
  V: Variant;
begin
  aggs := TAstaIOAggregates(Collection);
  astaDS := __TAstaIOCustomDataSet(DataSet);
  valObj := nil;
  cntObj := nil;
  if FValueIndex = -1 then
     FValueIndex := aggs.AllocValues;
  if (AggKind = akAvg) and (FCountIndex = -1) then
     FCountIndex := aggs.AllocValues;
 if (IndexName <> '') and (Level > 0) then begin
    ind := aggs.FIndexes.IndexByName(IndexName);
    opts := [];
    if ioDescending in ind.Options then
      Include(opts, crDescending);
    if ioCaseInsensitive in ind.Options then
      Include(opts, crCaseInsensitive);
    for i := 0 to ind.RecsCount - 1 do
      if astaDS.DirectFilter(ind.Recs[i]) then begin
        if (valObj = nil) or (ind.CompareRecs(ind.Recs[i], ind.Recs[i - 1], Level, opts) <> 0) then
          AllocAggVals(valObj, cntObj);
        AttachAggVals(ind.Recs[i], valObj, cntObj);
        if FAggField = nil then
          V := 1
        else
          V := astaDS.ReadField(ind.Recs[i], FAggField);
        IncAggVals(V, valObj, cntObj);
      end;
  end
  else begin
    for i := 0 to aggs.FRecs.Count - 1 do
      if astaDS.DirectFilter(aggs.FRecs[i]) then begin
        if valObj = nil then
          AllocAggVals(valObj, cntObj);
        AttachAggVals(aggs.FRecs[i], valObj, cntObj);
        if FAggField = nil then
          V := 1
        else
          V := astaDS.ReadField(aggs.FRecs[i], FAggField);
        IncAggVals(V, valObj, cntObj);
      end;
  end;
end;

procedure TAstaIOAggregate.Recalc;
begin
  Clear;
  if Running then begin
    Calculate;
    Include(FState, asActual);
  end
  else
    Exclude(FState, asActual);
end;

procedure TAstaIOAggregate.UpToDate;
begin
  if not Actual then begin
    Clear;
    Calculate;
    Include(FState, asActual);
  end;
end;

function TAstaIOAggregate.GetValue: Variant;
var
  aggs: TAstaIOAggregates;
  valObj: TAstaIOAggregateValue;
  cntObj: TAstaIOAggregateValue;
  prevDecimalSeparator: Char;
begin
  aggs := TAstaIOAggregates(Collection);
  Result := Null;
  if Running and
     aggs.FIndexes.IsPositionValid and (FValueIndex <> -1) and
     ((AggKind <> akAvg) or (FCountIndex <> -1)) and
     (DataSet.State in [dsBrowse, dsCalcFields, dsFilter, dsNewValue, dsOldValue,
                        dsCurValue, dsInternalCalc]) and
     ((IndexName = '') or (Level = 0) or aggs.FIndexes.IndexByName(IndexName).Selected) then begin
    DataSet.UpdateCursorPos;
    valObj := aggs.FIndexes.Rec.FAggs^[FValueIndex];
    if valObj <> nil then
      if AggKind = akAvg then begin
        cntObj := aggs.FIndexes.Rec.FAggs^[FCountIndex];
        if (cntObj <> nil) and
           not VarIsNull(valObj.Value) and not VarIsNull(cntObj.Value) and
           (cntObj.Value > 0) then begin
          prevDecimalSeparator := FormatSettings.DecimalSeparator;
          FormatSettings.DecimalSeparator := '.';
          try
            Result := valObj.Value / cntObj.Value;
          finally
            FormatSettings.DecimalSeparator := prevDecimalSeparator;
          end;
        end;
      end
      else
        Result := valObj.Value;
  end;
end;

procedure TAstaIOAggregate.RecordUpdating(ARec: TAstaDBListItem);
begin
  if Running then begin
    if not (AggKind in [akMax, akMin]) then begin
      FUpdatingRec := ARec;
      if not ((AggKind = akCount) and (FieldName = '')) then
        FUpdatingVal := __TAstaIOCustomDataSet(DataSet).ReadField(ARec, FAggField);
    end;
  end
  else
    Exclude(FState, asActual);
end;

procedure TAstaIOAggregate.RecordCheckUpdate(AField: TField);
var
  ind: TAstaIOIndex;
  aggs: TAstaIOAggregates;
begin
  if Running and not (asUpdating in FState) then begin
    aggs := TAstaIOAggregates(Collection);
    if (IndexName <> '') and (Level > 0) then begin
      ind := aggs.FIndexes.IndexByName(IndexName);
      if (AField.FieldKind = fkData) and (ind.FFieldList.IndexOfObject(AField) <> -1) then
        Include(FState, asUpdating);
    end;
    if (FAggField <> nil) and (FAggField.FieldKind <> fkData) or (FAggField = AField) then
      Include(FState, asUpdating);
  end;
end;

procedure TAstaIOAggregate.RecordUpdated(ARec: TAstaDBListItem);
var
  valObj: TAstaIOAggregateValue;
  cntObj: TAstaIOAggregateValue;
begin
  if Running then begin
    if asUpdating in FState then
      if not (AggKind in [akMax, akMin]) then begin
        GetAggVals(FUpdatingRec, valObj, cntObj);
        if valObj <> nil then
          DecAggVals(FUpdatingVal, valObj, cntObj);
        DetachAggVals(FUpdatingRec);
        RecordInserted(ARec);
      end
      else
        Recalc;
  end
  else
    Exclude(FState, asActual);
  Exclude(FState, asUpdating);
end;

procedure TAstaIOAggregate.RecordCancel(ARec: TAstaDBListItem);
begin
  FUpdatingRec := nil;
  FUpdatingVal := Unassigned;
  Exclude(FState, asUpdating);
end;

procedure TAstaIOAggregate.RecordInserted(ARec: TAstaDBListItem);
var
  i, i1: Integer;
  valObj: TAstaIOAggregateValue;
  cntObj: TAstaIOAggregateValue;
  foundGroup: TAstaDBListItem;
  ind: TAstaIOIndex;
  aggs: TAstaIOAggregates;
  astaDS: __TAstaIOCustomDataSet;
  opts: TAstaIOIndexCompareRecOptions;
  V: Variant;
begin
  if Running then begin
    aggs := TAstaIOAggregates(Collection);
    astaDS := __TAstaIOCustomDataSet(DataSet);
    if astaDS.DirectFilter(ARec) then begin
      foundGroup := nil;
      i1 := -1;
      if (IndexName <> '') and (Level > 0) then begin
        ind := aggs.FIndexes.IndexByName(IndexName);
        if ind.LocateByBookmark(ARec.FBookMark, [], [], i) = 0 then begin
          opts := [];
          if ioDescending in ind.Options then
            Include(opts, crDescending);
          if ioCaseInsensitive in ind.Options then
            Include(opts, crCaseInsensitive);
          i1 := i - 1;
          while (foundGroup = nil) and (i1 >= 0) and
                (ind.CompareRecs(ind.Recs[i1], ind.Recs[i1 + 1], Level, opts) = 0) do
            if astaDS.DirectFilter(ind.Recs[i1]) then
              foundGroup := ind.Recs[i1]
            else
              Dec(i1);
          if foundGroup = nil then begin
            i1 := i + 1;
            while (foundGroup = nil) and (i1 < ind.RecsCount) and
                  (ind.CompareRecs(ind.Recs[i1], ind.Recs[i1 - 1], Level, opts) = 0) do
              if astaDS.DirectFilter(ind.Recs[i1]) then
                foundGroup := ind.Recs[i1]
              else
                Inc(i1);
          end;
        end;
      end
      else begin
        i1 := 0;
        while (i1 < aggs.FRecs.Count) and not astaDS.DirectFilter(aggs.FRecs[i1]) do
          Inc(i1);
        if i1 < aggs.FRecs.Count then
          foundGroup := aggs.FRecs[i1];
      end;
      if i1 = -1 then
        Exclude(FState, asActual)
      else begin
        valObj := nil;
        cntObj := nil;
        if foundGroup <> nil then
          GetAggVals(foundGroup, valObj, cntObj);
        if valObj = nil then
          AllocAggVals(valObj, cntObj);
        AttachAggVals(ARec, valObj, cntObj);
        if FAggField = nil then
          V := 1
        else
          V := astaDS.ReadField(ARec, FAggField);
        IncAggVals(V, valObj, cntObj);
      end;
    end;
  end
  else
    Exclude(FState, asActual);
end;

procedure TAstaIOAggregate.RecordDeleted(ARec: TAstaDBListItem);
var
  valObj: TAstaIOAggregateValue;
  cntObj: TAstaIOAggregateValue;
  V: Variant;
  astaDS: __TAstaIOCustomDataset;
begin
  if Running then begin
    GetAggVals(ARec, valObj, cntObj);
    if valObj <> nil then begin
      astaDS := __TAstaIOCustomDataset(DataSet);
      if FAggField = nil then
        V := 1
      else
        V := astaDS.ReadField(ARec, FAggField);
      DecAggVals(V, valObj, cntObj);
    end;
    DetachAggVals(ARec);
  end
  else
    Exclude(FState, asActual);
end;

procedure TAstaIOAggregate.LoadFromStream(AStream: TStream);
begin
  Clear;
  FieldsUnDefined;
  ReadString(AStream);
  FName := ReadString(AStream);
  FFieldName := ReadString(AStream);
  AStream.ReadBuffer(FAggKind, SizeOf(FAggKind));
  FIndexName := ReadString(AStream);
  AStream.ReadBuffer(FLevel, SizeOf(FLevel));
  AStream.ReadBuffer(FActive, SizeOf(FActive));
  FieldsDefined;
end;

procedure TAstaIOAggregate.SaveToStream(AStream: TStream);
begin
  WriteString(AStream, ClassName);
  WriteString(AStream, FName);
  WriteString(AStream, FFieldName);
  AStream.WriteBuffer(FAggKind, SizeOf(FAggKind));
  WriteString(AStream, FIndexName);
  AStream.WriteBuffer(FLevel, SizeOf(FLevel));
  AStream.WriteBuffer(FActive, SizeOf(FActive));
end;

{ TAstaIOAggregates }

constructor TAstaIOAggregates.Create(AIndexes: TAstaIOIndexes; ARecs: TAstaDBList);
begin
  inherited Create(TAstaIOAggregate);
  FIndexes := AIndexes;
  FRecs := ARecs;
  FActive := True;
  FValuesPool := TBits.Create;
  //FValuesPool.Size := 256; // Original Asta
  FValuesPool.Size := 1024; // ed - 12/12/2011  
  FAggCellsAllocated := 0;
  if ARecs <> nil then
    DBListCreated;
end;

destructor TAstaIOAggregates.Destroy;
begin
  Active := False;
  FValuesPool.Free;
  FValuesPool := nil;
  inherited Destroy;
end;

function TAstaIOAggregates.GetOwner: TPersistent;
begin
  if FIndexes <> nil then
    Result := FIndexes.FDataSet
  else
    Result := nil;
end;

function TAstaIOAggregates.GetItems(AIndex: Integer): TAstaIOAggregate;
begin
  Result := TAstaIOAggregate(inherited Items[AIndex]);
end;

function TAstaIOAggregates.Add: TAstaIOAggregate;
begin
  Result := TAstaIOAggregate(inherited Add);
end;

function TAstaIOAggregates.FindAggregate(const AName: String): TAstaIOAggregate;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].Name, AName) = 0 then begin
      Result := Items[i];
      Break;
    end;
end;

function TAstaIOAggregates.AggregateByName(const AName: String): TAstaIOAggregate;
begin
  Result := FindAggregate(AName);
  if Result = nil then
    raise Exception.CreateFmt(SAggNotFound, [AName]);
end;

function TAstaIOAggregates.AllocValues: Integer;
var
  i: Integer;
begin
  Result := FValuesPool.OpenBit;
  if Result = FValuesPool.Size then
    raise Exception.Create(STooManyAggs);
  FValuesPool.Bits[Result] := True;
  if Result >= FAggCellsAllocated then begin
    for i := 0 to FRecs.Count - 1 do begin
      ReallocMem(FRecs[i].FAggs, (Result + 4) * SizeOf(Pointer));
      FillChar(FRecs[i].FAggs^[FAggCellsAllocated],
        (Result + 4 - FAggCellsAllocated) * SizeOf(Pointer), #0);
    end;
    FAggCellsAllocated := Result + 4;
  end;
  for i := 0 to FRecs.Count - 1 do
    FRecs[i].FAggs^[Result] := nil;
end;

procedure TAstaIOAggregates.DeleteValues(var AIndex: Integer);
var
  i: Integer;
begin
  try
    for i := 0 to FRecs.Count - 1 do
      if FRecs[i].FAggs^[AIndex] <> nil then begin
        TAstaIOAggregateValue(FRecs[i].FAggs^[AIndex]).Detach;
        FRecs[i].FAggs^[AIndex] := nil;
      end;
  finally
    FValuesPool.Bits[AIndex] := False;
    AIndex := -1;
  end;
end;

procedure TAstaIOAggregates.Recalc;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Recalc;
end;

procedure TAstaIOAggregates.RecalcOnIndex(const AIndexName: String);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].IndexName, AIndexName) = 0 then
      Items[i].Recalc;
end;

procedure TAstaIOAggregates.UpToDate;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].UpToDate;
end;

procedure TAstaIOAggregates.SetActive(const Value: Boolean);
begin
  if Active <> Value then begin
    FActive := Value;
    Recalc;
  end;
end;

procedure TAstaIOAggregates.FieldsDefined;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].FieldsDefined;
end;

procedure TAstaIOAggregates.FieldsUnDefined;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].FieldsUnDefined;
end;

procedure TAstaIOAggregates.DBListCreated;
begin
  FRecs := (TAstaIOCustomDataSet(FIndexes.FDataSet)).FAstaList;
  FRecs.FAggOnAdd := DoRecAdded;
  FRecs.FAggOnDelete := DoRecDeleted;
  Recalc;
end;

procedure TAstaIOAggregates.DoRecAdded(ARec: TAstaDBListItem);
begin
  if FAggCellsAllocated > 0 then begin
    GetMem(ARec.FAggs, FAggCellsAllocated * SizeOf(Pointer));
    FillChar(ARec.FAggs^[0], FAggCellsAllocated * SizeOf(Pointer), #0);
  end;
end;

procedure TAstaIOAggregates.DoRecDeleted(ARec: TAstaDBListItem);
var
  i: Integer;
begin
  if FAggCellsAllocated > 0 then begin
    for i := 0 to FAggCellsAllocated - 1 do
      if ARec.FAggs^[i] <> nil then begin
        TAstaIOAggregateValue(ARec.FAggs^[i]).Detach;
        ARec.FAggs^[i] := nil;
      end;
    FreeMem(ARec.FAggs, FAggCellsAllocated * SizeOf(Pointer));
    ARec.FAggs := nil;
  end;
end;

procedure TAstaIOAggregates.RecordCancel(ARec: TAstaDBListItem);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].RecordCancel(ARec);
end;

procedure TAstaIOAggregates.RecordCheckUpdate(AField: TField);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].RecordCheckUpdate(AField);
end;

procedure TAstaIOAggregates.RecordDeleted(ARec: TAstaDBListItem);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].RecordDeleted(ARec);
end;

procedure TAstaIOAggregates.RecordInserted(ARec: TAstaDBListItem);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].RecordInserted(ARec);
end;

procedure TAstaIOAggregates.RecordUpdated(ARec: TAstaDBListItem);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].RecordUpdated(ARec);
end;

procedure TAstaIOAggregates.RecordUpdating(ARec: TAstaDBListItem);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].RecordUpdating(ARec);
end;

procedure TAstaIOAggregates.LoadFromStream(AStream: TStream);
var
  i: Integer;
  strPos: LongInt;
  stop: Boolean;
begin
  Clear;
  strPos := AStream.Position;
  try
    stop := not ReadCheckString(AStream, ClassName);
  except
    stop := True;
  end;
  if stop then
    AStream.Position := strPos
  else begin
    AStream.ReadBuffer(FActive, SizeOf(FActive));
    AStream.ReadBuffer(i, SizeOf(i));
    while i > 0 do begin
      Add.LoadFromStream(AStream);
      Dec(i);
    end;
  end;
end;

procedure TAstaIOAggregates.SaveToStream(AStream: TStream);
var
  i: Integer;
begin
  WriteString(AStream, ClassName);
  AStream.WriteBuffer(FActive, SizeOf(FActive));
  i := Count;
  AStream.WriteBuffer(i, SizeOf(i));
  for i := 0 to Count - 1 do
    Items[i].SaveToStream(AStream);
end;

end.

